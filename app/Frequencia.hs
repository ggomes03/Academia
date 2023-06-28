module Frequencia (main) where

import Lib
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.String (fromString)
import Graphics.UI.Gtk hiding (set)
import qualified Graphics.UI.Gtk as Gtk
import Control.Applicative  
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Monad (void)
import Text.Read (readMaybe)
import System.IO.Error (ioeGetErrorString)

import Tipos


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
        [cellText Gtk.:= show (idAlunoFrequen row)]
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
    runApp
    

runApp :: IO ()
runApp = do 

    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]

    buttonInsert <- buttonNewWithLabel "Inserir nova frequencia"
    widgetSetSizeRequest buttonInsert 100 50

    box <- vBoxNew False 10
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxHomogeneous Gtk.:= True, boxSpacing Gtk.:= 10]

    buttonInsert `on` buttonActivated $ do 
        mainInsert

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



mainInsert :: IO ()
mainInsert = do
    void initGUI

    window <- windowNew
    Gtk.set window [ windowTitle Gtk.:= "Academia", containerBorderWidth Gtk.:= 10 ]

    -- Criar widgets
    idFrequenciaEntry <- entryNew
    idAlunoEntry <- entryNew
    dataFrequenciaEntry <- entryNew
    indicPresenEntry <- entryNew
    inserirButton <- buttonNewWithLabel "Inserir"

    -- Criar layout
    vbox <- vBoxNew False 10
    containerAdd window vbox
    table <- tableNew 5 2 False
    boxPackStart vbox table PackGrow 0
    boxPackStart vbox inserirButton PackNatural 0

    -- Adicionar labels e entries à tabela
    addLabelAndEntry table 0 "ID Frequencia:" idFrequenciaEntry
    addLabelAndEntry table 1 "ID Aluno:" idAlunoEntry
    addLabelAndEntry table 2 "Data Frequencia:" dataFrequenciaEntry
    addLabelAndEntry table 3 "Presente: S ou N" indicPresenEntry
    
    inserirButton `on` buttonActivated $ do
        idFrequenciaStr <- entryGetText idFrequenciaEntry
        idAlunoStr <- entryGetText idAlunoEntry
        dataFrequencia <- entryGetText dataFrequenciaEntry >>= return . fromString
        indicPresen <- entryGetText indicPresenEntry

        let maybeIdFrequencia = readMaybe idFrequenciaStr :: Maybe Int
            maybeIdAluno = readMaybe idAlunoStr :: Maybe Int

        case (maybeIdFrequencia, maybeIdAluno) of
            (Just idFrequencia, Just idAluno) -> do
                let frequencia = Frequencia { idFrequencia = idFrequencia, idAlunoFrequen = idAluno, dataFrequen = dataFrequencia, indicPresen = indicPresen }
                conn <- open "db/academia.sqlite"
                let query = fromString "INSERT INTO Frequencia (idFrequencia, idAluno, dataFreq, indicPresen) VALUES (?, ?, ?, ?)" :: Query
                execute conn query (idFrequencia, idAluno, dataFrequencia, indicPresen)
                close conn
                putStrLn "Inserido com sucesso!"
            _ -> do
                dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal, Gtk.DialogDestroyWithParent] Gtk.MessageError Gtk.ButtonsOk "Erro: Valores inválidos nos campos 'ID Frequencia' ou 'ID Aluno'."
                Gtk.dialogRun dialog
                Gtk.widgetDestroy dialog

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
