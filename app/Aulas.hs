module Aulas (main) where

import Lib
import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow
import Graphics.UI.Gtk hiding (set)
import qualified Graphics.UI.Gtk as Gtk
import Control.Applicative  
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Monad (void)

import Tipos


main :: IO ()
main = do
    void initGUI
    runApp


createTable :: [Aulas] -> IO Widget
createTable aulas = do
    -- Cria o TreeView
    treeView <- treeViewNew

    -- Cria as colunas da tabela
    columnIdAula <- treeViewColumnNew
    columnNomeAula <- treeViewColumnNew
    columnNomeInstrutor <- treeViewColumnNew
    columnHorarioAula <- treeViewColumnNew
    

    -- Adiciona as colunas ao TreeView
    treeViewAppendColumn treeView columnIdAula
    treeViewAppendColumn treeView columnNomeAula
    treeViewAppendColumn treeView columnNomeInstrutor
    treeViewAppendColumn treeView columnHorarioAula
    
    -- Define os títulos das colunas
    treeViewColumnSetTitle columnIdAula "Id"
    treeViewColumnSetTitle columnNomeAula "Aula"
    treeViewColumnSetTitle columnNomeInstrutor "Instrutor"
    treeViewColumnSetTitle columnHorarioAula "Horario"
    

    -- Cria o ListStore para armazenar os dados
    store <- listStoreNew aulas

    -- Cria os CellRenderers para exibir os dados nas colunas
    rendererIdAula <- cellRendererTextNew
    rendererNomeAula <- cellRendererTextNew
    rendererNomeInstrutor <- cellRendererTextNew
    rendererHorarioAula <- cellRendererTextNew
    

    -- Adiciona os CellRenderers às colunas
    treeViewColumnPackStart columnIdAula rendererIdAula True
    treeViewColumnPackStart columnNomeAula rendererNomeAula True
    treeViewColumnPackStart columnNomeInstrutor rendererNomeInstrutor True
    treeViewColumnPackStart columnHorarioAula rendererHorarioAula True
    

    -- Define as propriedades dos CellRenderers
    cellLayoutSetAttributes columnIdAula rendererIdAula store $ \row ->
        [cellText Gtk.:= show (idAula row)]
    cellLayoutSetAttributes columnNomeAula rendererNomeAula store $ \row ->
        [cellText Gtk.:= show (nomeAula row)]
    cellLayoutSetAttributes columnNomeInstrutor rendererNomeInstrutor store $ \row ->
        [cellText Gtk.:= show (nomeInstrutor row)]    
    cellLayoutSetAttributes columnHorarioAula rendererHorarioAula store $ \row ->
        [cellText Gtk.:= show (horarioAula row)] 

    -- Define o modelo de dados do TreeView
    treeViewSetModel treeView (Just store)

    -- Retorna o TreeView como Widget
    return $ toWidget treeView


runApp :: IO ()
runApp = do
    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]

    buttonInsert <- buttonNewWithLabel "Inserir nova Aula"
    widgetSetSizeRequest buttonInsert 100 50

    box <- vBoxNew False 10
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxHomogeneous Gtk.:= True, boxSpacing Gtk.:= 10]


    buttonInsert `on` buttonActivated $ do
        widgetDestroy window
        mainInsert

    -- Cria a tabela
    conn <- open "db/academia.sqlite"
    let query = fromString "SELECT * FROM Aulas" :: Query
    results <- query_ conn query :: IO [Aulas]
    table <- createTable results
    close conn

    -- Adiciona a tabela e o botao de insert à box
    containerAdd box buttonInsert
    containerAdd box table

    containerAdd window box

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    widgetShowAll window
    mainGUI

mainInsert :: IO ()
mainInsert = do
    void initGUI

    window <- windowNew
    Gtk.set window [ windowTitle Gtk.:= "Academia", containerBorderWidth Gtk.:= 10 ]

    -- Criar widgets
    idAulaEntry <- entryNew
    nomeEntry <- entryNew
    nomeInstrutorEntry <- entryNew
    horarioAulaEntry <- entryNew
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
    addLabelAndEntry table 0 "ID:" idAulaEntry
    addLabelAndEntry table 1 "Nome:" nomeEntry
    addLabelAndEntry table 2 "Instrutor:" nomeInstrutorEntry
    addLabelAndEntry table 3 "Hora:" horarioAulaEntry

    inserirButton `on` buttonActivated $ do
        idAulaStr <- entryGetText idAulaEntry
        let idAula = read idAulaStr :: Int
        
        nomeAulaStr <- entryGetText nomeEntry
        let nomeAula = fromString nomeAulaStr
        
        nomeInstrutorStr <- entryGetText nomeInstrutorEntry
        let nomeInstrutor = fromString nomeInstrutorStr
        
        horarioAulaStr <- entryGetText horarioAulaEntry
        let horarioAula = read horarioAulaStr :: Int
        
        if any null [idAulaStr, nomeAulaStr, nomeInstrutorStr, horarioAulaStr]
            then do 
                dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal, Gtk.DialogDestroyWithParent] Gtk.MessageError Gtk.ButtonsOk "Erro: Nenhum campo pode estar vazio"
                Gtk.dialogRun dialog
                Gtk.widgetDestroy dialog
                putStrLn "Erro: Todos os campos devem ser preenchidos."
            else do
                let aula = Aulas { idAula = idAula, nomeAula = nomeAula, nomeInstrutor = nomeInstrutor, horarioAula = horarioAula }
                conn <- open "db/academia.sqlite"
                let query = fromString "INSERT INTO Aulas (idAula, nomeAula, nomeInstrutor, horarioAula) VALUES (?, ?, ?, ?)" :: Query
                execute conn query (idAula, nomeAula, nomeInstrutor, horarioAula)
                close conn
                dialog <- messageDialogNew Nothing [] MessageInfo ButtonsClose "Aula Inserida!"
                dialogRun dialog
                widgetDestroy dialog
                putStrLn "Inserido com sucesso!"

    backButton `on` buttonActivated $ do
        widgetDestroy window
        liftIO mainQuit
        main

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

