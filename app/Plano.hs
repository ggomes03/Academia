module Plano (main) where

import Lib
import Control.Applicative
import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow
import Graphics.UI.Gtk hiding (set)
import qualified Graphics.UI.Gtk as Gtk
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

import Tipos
import PlanoInsert

-- Função para criar a tabela com os dados
createTable :: [Planos] -> IO Widget
createTable planos = do
    -- Cria o TreeView
    treeView <- treeViewNew

    -- Cria as colunas da tabela
    columnId <- treeViewColumnNew
    columnNome <- treeViewColumnNew
    columnDescricao <- treeViewColumnNew
    columnPreco <- treeViewColumnNew

    -- Adiciona as colunas ao TreeView
    treeViewAppendColumn treeView columnId
    treeViewAppendColumn treeView columnNome
    treeViewAppendColumn treeView columnDescricao
    treeViewAppendColumn treeView columnPreco

    -- Define os títulos das colunas
    treeViewColumnSetTitle columnId "Id"
    treeViewColumnSetTitle columnNome "Nome"
    treeViewColumnSetTitle columnDescricao "Descrição"
    treeViewColumnSetTitle columnPreco "Preço"

    -- Cria o ListStore para armazenar os dados
    store <- listStoreNew planos

    -- Cria os CellRenderers para exibir os dados nas colunas
    rendererId <- cellRendererTextNew
    rendererNome <- cellRendererTextNew
    rendererDescricao <- cellRendererTextNew
    rendererPreco <- cellRendererTextNew

    -- Adiciona os CellRenderers às colunas
    treeViewColumnPackStart columnId rendererId True
    treeViewColumnPackStart columnNome rendererNome True
    treeViewColumnPackStart columnDescricao rendererDescricao True
    treeViewColumnPackStart columnPreco rendererPreco True

    -- Define as propriedades dos CellRenderers
    cellLayoutSetAttributes columnId rendererId store $ \row ->
        [cellText Gtk.:= show (idPlano row)]
    cellLayoutSetAttributes columnNome rendererNome store $ \row ->
        [cellText Gtk.:= nomePlano row]
    cellLayoutSetAttributes columnDescricao rendererDescricao store $ \row ->
        [cellText Gtk.:= descricao row]
    cellLayoutSetAttributes columnPreco rendererPreco store $ \row ->
        [cellText Gtk.:= show (preco row)]

    -- Define o modelo de dados do TreeView
    treeViewSetModel treeView (Just store)

    -- Retorna o TreeView como Widget
    return $ toWidget treeView

main :: IO ()
main = do
    void initGUI

    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]

    buttonInsert <- buttonNewWithLabel "Inserir novo plano"
    widgetSetSizeRequest buttonInsert 100 50

    box <- vBoxNew False 10
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxHomogeneous Gtk.:= True, boxSpacing Gtk.:= 10]

    buttonInsert `on` buttonActivated $ do 
        PlanoInsert.mainIns

    -- Cria a tabela
    conn <- open "db/academia.sqlite"
    let query = fromString "SELECT * FROM Planos" :: Query
    results <- query_ conn query :: IO [Planos]
    table <- createTable results
    close conn

    -- Adiciona a tabela e o botao de insert à box
    containerAdd box buttonInsert
    containerAdd box table

    containerAdd window box
    
    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    -- widgetShow buttonInsert
    widgetShowAll window
    mainGUI