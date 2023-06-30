module Alunos (main) where

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
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Graphics.UI.Gtk hiding (Action, backspace)

import Tipos

main :: IO ()
main = do
    void initGUI
    runApp


-- Função para criar a tabela com os dados
createTable :: [Aluno] -> IO Widget
createTable alunos = do
    -- Cria o TreeView
    treeView <- treeViewNew
    -- Cria as colunas da tabela
    columnId <- treeViewColumnNew
    columnNome <- treeViewColumnNew
    columnDataNasc <- treeViewColumnNew
    columnEmail <- treeViewColumnNew
    columnFone <- treeViewColumnNew

    -- Adiciona as colunas ao TreeView
    treeViewAppendColumn treeView columnId
    treeViewAppendColumn treeView columnNome
    treeViewAppendColumn treeView columnDataNasc
    treeViewAppendColumn treeView columnEmail
    treeViewAppendColumn treeView columnFone

    -- Define os títulos das colunas
    treeViewColumnSetTitle columnId "Id"
    treeViewColumnSetTitle columnNome "Nome"
    treeViewColumnSetTitle columnDataNasc "Data Nascimento"
    treeViewColumnSetTitle columnEmail "Email"
    treeViewColumnSetTitle columnFone "Fone"

    -- Cria o ListStore para armazenar os dados
    store <- listStoreNew alunos

    -- Cria os CellRenderers para exibir os dados nas colunas
    rendererId <- cellRendererTextNew
    rendererNome <- cellRendererTextNew
    rendererDataNasc <- cellRendererTextNew
    rendererEmail <- cellRendererTextNew
    rendererFone <- cellRendererTextNew

    -- Adiciona os CellRenderers às colunas
    treeViewColumnPackStart columnId rendererId True
    treeViewColumnPackStart columnNome rendererNome True
    treeViewColumnPackStart columnDataNasc rendererDataNasc True
    treeViewColumnPackStart columnEmail rendererEmail True
    treeViewColumnPackStart columnFone rendererFone True

    -- Define as propriedades dos CellRenderers
    cellLayoutSetAttributes columnId rendererId store $ \row ->
        [cellText Gtk.:= show (idAluno row)]
    cellLayoutSetAttributes columnNome rendererNome store $ \row ->
        [cellText Gtk.:= nome row]
    cellLayoutSetAttributes columnDataNasc rendererDataNasc store $ \row ->
        [cellText Gtk.:= dataNascimento row]
    cellLayoutSetAttributes columnEmail rendererEmail store $ \row ->
        [cellText Gtk.:= email row]
    cellLayoutSetAttributes columnFone rendererFone store $ \row ->
        [cellText Gtk.:= fone row]

    -- Define o modelo de dados do TreeView
    treeViewSetModel treeView (Just store)

    -- Retorna o TreeView como Widget
    return $ toWidget treeView

    
runApp :: IO()
runApp = do 
    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]

    buttonInsert <- buttonNewWithLabel "Inserir novo Aluno"
        
    box <- vBoxNew False 0 
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxSpacing Gtk.:= 1]

    conn <- open "db/academia.sqlite"
    let query = fromString "SELECT * FROM Alunos" :: Query
    results <- query_ conn query :: IO [Aluno]
    table <- createTable results
    close conn 

    -- containerAdd box buttonInsert
    containerAdd box table
    boxPackStart box buttonInsert PackNatural 0
    containerAdd window box

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False 

    buttonInsert `on` buttonActivated $ do
        widgetDestroy window
        mainInsert
    
    widgetShowAll window
    mainGUI

mainInsert :: IO ()
mainInsert = do
    void initGUI

    window <- windowNew
    Gtk.set window [ windowTitle Gtk.:= "Academia", containerBorderWidth Gtk.:= 10 ]

    -- Criar widgets
    nomeEntry <- entryNew
    dataNascimentoEntry <- entryNew
    emailEntry <- entryNew
    foneEntry <- entryNew
    inserirButton <- buttonNewWithLabel "Inserir"
    backButton <- buttonNewWithLabel "Voltar"

    -- Criar layout
    vbox <- vBoxNew False 10
    containerAdd window vbox
    table <- tableNew 5 2 False
    boxPackStart vbox table PackGrow 0

    boxPackStart vbox inserirButton PackNatural 0
    boxPackStart vbox backButton PackNatural 0

    -- Adicionar labels e entries à tabela
    addLabelAndEntry table 1 "Nome:" nomeEntry
    addLabelAndEntry table 2 "Data de Nascimento:" dataNascimentoEntry
    addLabelAndEntry table 3 "Email:" emailEntry
    addLabelAndEntry table 4 "Telefone:" foneEntry

    inserirButton `on` buttonActivated $ do
        nome <- entryGetText nomeEntry >>= return . fromString
        dataNascimento <- entryGetText dataNascimentoEntry >>= return . fromString
        email <- entryGetText emailEntry >>= return . fromString
        fone <- entryGetText foneEntry >>= return . fromString

        if any null [nome, dataNascimento, email, fone]
            then do
                -- Mostrar um diálogo de erro
                dialog <- messageDialogNew Nothing [] MessageError ButtonsClose "Por favor, preencha todos os campos!"
                dialogRun dialog
                widgetDestroy dialog
            else do
                -- let idAluno = read idAlunoText :: Int
                let aluno = Aluno { idAluno = 0, nome = nome, dataNascimento = dataNascimento, email = email, fone = fone }
                conn <- open "db/academia.sqlite"
                let query = fromString "INSERT INTO Alunos (nome, dataNascimento, email, fone) VALUES (?, ?, ?, ?)" :: Query
                execute conn query (nome, dataNascimento, email, fone)
                close conn

                dialog <- messageDialogNew Nothing [] MessageInfo ButtonsClose "Aluno Inserido!"
                dialogRun dialog
                widgetDestroy dialog

    backButton `on` buttonActivated $ do
        widgetDestroy window
        main

    -- Configurar ação do fechamento da janela
    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    widgetShowAll window
    mainGUI
