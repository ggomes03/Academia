module Plano (main) where

import Lib
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Monad (void)
import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow
import Graphics.UI.Gtk hiding (set)
import qualified Graphics.UI.Gtk as Gtk

import Tipos

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
    runApp

runApp :: IO ()
runApp = do 
    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]

    buttonInsert <- buttonNewWithLabel "Inserir novo plano"
    widgetSetSizeRequest buttonInsert 100 50

    box <- vBoxNew False 10
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxHomogeneous Gtk.:= True, boxSpacing Gtk.:= 10]

    buttonInsert `on` buttonActivated $ do 
        mainInsert

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

mainInsert :: IO ()
mainInsert = do
    void initGUI

    window <- windowNew
    Gtk.set window [ windowTitle Gtk.:= "Academia", containerBorderWidth Gtk.:= 10 ]

    -- Criar widgets
    idPlanoEntry <- entryNew
    nomePlanoEntry <- entryNew
    descricaoEntry <- entryNew
    precoEntry <- entryNew
    inserirButton <- buttonNewWithLabel "Inserir"

    -- Criar layout
    vbox <- vBoxNew False 10
    containerAdd window vbox
    table <- tableNew 5 2 False
    boxPackStart vbox table PackGrow 0
    boxPackStart vbox inserirButton PackNatural 0

    -- Adicionar labels e entries à tabela
    addLabelAndEntry table 0 "ID:" idPlanoEntry
    addLabelAndEntry table 1 "Nome:" nomePlanoEntry
    addLabelAndEntry table 2 "Descrição:" descricaoEntry
    addLabelAndEntry table 3 "Preço:" precoEntry

    inserirButton `on` buttonActivated $ do
        idPlanoText <- entryGetText idPlanoEntry
        nomePlano <- entryGetText nomePlanoEntry >>= return . fromString
        descricao <- entryGetText descricaoEntry >>= return . fromString
        precoText <- entryGetText precoEntry

        if any null [idPlanoText, nomePlano, descricao, precoText]
            then do
                -- Mostrar um diálogo de erro
                dialog <- messageDialogNew Nothing [] MessageError ButtonsClose "Por favor, preencha todos os campos!"
                dialogRun dialog
                widgetDestroy dialog
            else do
                let idPlano = read idPlanoText :: Int
                    preco = read precoText :: Float
                    plano = Planos { idPlano = idPlano, nomePlano = nomePlano, descricao = descricao, preco = preco }
                conn <- open "db/academia.sqlite"
                let query = fromString "INSERT INTO Planos (idPlano, nome, descricao, preco) VALUES (?, ?, ?, ?)" :: Query
                execute conn query (idPlano, nomePlano, descricao, preco)
                close conn

                dialog <- messageDialogNew Nothing [] MessageInfo ButtonsClose "Plano Inserido!"
                dialogRun dialog
                widgetDestroy dialog
                putStrLn "Inserido com sucesso!"


    -- Configurar ação do fechamento da janela
    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    widgetShowAll window
    mainGUI

-- Função auxiliar para adicionar uma label e uma entry a uma tabela
addLabelAndEntry :: Table -> Int -> String -> Entry -> IO ()
addLabelAndEntry table row label entry = do
    labelWidget <- labelNew (Just label)
    miscSetAlignment labelWidget 0 0.5
    tableAttachDefaults table labelWidget 0 1 row (row + 1)
    tableAttachDefaults table entry 1 2 row (row + 1)

