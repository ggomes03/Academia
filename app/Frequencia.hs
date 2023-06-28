module Frequencia (main) where

import Lib
import Control.Applicative  
import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow
import Graphics.UI.Gtk hiding (set)
import qualified Graphics.UI.Gtk as Gtk
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Monad (void)

import FrequenciaInsert

data Frequencia = Frequencia {idFrequencia :: Int, idAluno :: Int, dataFrequen :: String, indicPresen :: String }

instance FromRow Frequencia where
    fromRow = Frequencia <$> field <*> field <*> field <*> field

instance Show Frequencia where
    show (Frequencia idFrequencia idAluno dataFrequen indicPresen) =
        "Plano {Id = " ++ show idFrequencia ++ 
        ", nome = " ++ show idAluno ++
        ", descricao = " ++ show dataFrequen ++
        ", preco = " ++ show indicPresen ++ "}\n"

-- Função para criar a tabela com os dados
createTable :: [Frequencia] -> IO Widget
createTable frequencia = do
    -- Cria o TreeView
    treeView <- treeViewNew

    -- Cria as colunas da tabela
    columnIdFrequencia <- treeViewColumnNew
    columnIdAluno <- treeViewColumnNew
    columnDataFrequencia <- treeViewColumnNew
    columnIndicPresen <- treeViewColumnNew

    -- Adiciona as colunas ao TreeView
    treeViewAppendColumn treeView columnIdFrequencia
    treeViewAppendColumn treeView columnIdAluno
    treeViewAppendColumn treeView columnDataFrequencia
    treeViewAppendColumn treeView columnIndicPresen

    -- Define os títulos das colunas
    treeViewColumnSetTitle columnIdFrequencia "Id Frequencia"
    treeViewColumnSetTitle columnIdAluno "Id Aluno"
    treeViewColumnSetTitle columnDataFrequencia "Data"
    treeViewColumnSetTitle columnIndicPresen "Situação"

    -- Cria o ListStore para armazenar os dados
    store <- listStoreNew frequencia

    -- Cria os CellRenderers para exibir os dados nas colunas
    rendererIdFrequencia <- cellRendererTextNew
    rendererIdAluno <- cellRendererTextNew
    rendererDataFrequencia <- cellRendererTextNew
    rendererIndicPresen <- cellRendererTextNew

    -- Adiciona os CellRenderers às colunas
    treeViewColumnPackStart columnIdFrequencia rendererIdFrequencia True
    treeViewColumnPackStart columnIdAluno rendererIdAluno True
    treeViewColumnPackStart columnDataFrequencia rendererDataFrequencia True
    treeViewColumnPackStart columnIndicPresen rendererIndicPresen True

    -- Define as propriedades dos CellRenderers
    cellLayoutSetAttributes columnIdFrequencia rendererIdFrequencia store $ \row ->
        [cellText Gtk.:= show (idFrequencia row)]
    cellLayoutSetAttributes columnIdAluno rendererIdAluno store $ \row ->
        [cellText Gtk.:= show (idAluno row)]
    cellLayoutSetAttributes columnDataFrequencia rendererDataFrequencia store $ \row ->
        [cellText Gtk.:= dataFrequen row]
    cellLayoutSetAttributes columnIndicPresen rendererIndicPresen store $ \row ->
        [cellText Gtk.:= indicPresen row]

    -- Define o modelo de dados do TreeView
    treeViewSetModel treeView (Just store)

    -- Retorna o TreeView como Widget
    return $ toWidget treeView

main :: IO ()
main = do
    void initGUI

    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]

    buttonInsert <- buttonNewWithLabel "Inserir nova frequencia"
    widgetSetSizeRequest buttonInsert 100 50

    box <- vBoxNew False 10
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxHomogeneous Gtk.:= True, boxSpacing Gtk.:= 10]

    buttonInsert `on` buttonActivated $ do 
        FrequenciaInsert.mainIns

    -- Cria a tabela
    conn <- open "db/academia.sqlite"
    let query = fromString "SELECT * FROM Frequencia" :: Query
    results <- query_ conn query :: IO [Frequencia]
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
