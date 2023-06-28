module FrequenciaInsert where

import Lib
import Control.Applicative  
import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow
import Graphics.UI.Gtk hiding (set)
import qualified Graphics.UI.Gtk as Gtk
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Monad (void)
import Text.Read (readMaybe)
import System.IO.Error (ioeGetErrorString)


data FrequenciaIns = FrequenciaIns {idFrequenciaIns :: Int, idAlunoIns :: Int, dataFrequenIns :: String, indicPresenIns :: String }

instance FromRow FrequenciaIns where
    fromRow = FrequenciaIns <$> field <*> field <*> field <*> field

instance Show FrequenciaIns where
    show (FrequenciaIns idFrequenciaIns idAlunoIns dataFrequenIns indicPresenIns) =
        "Plano {Id = " ++ show idFrequenciaIns ++ 
        ", nome = " ++ show idAlunoIns ++
        ", descricao = " ++ show dataFrequenIns ++
        ", preco = " ++ show indicPresenIns ++ "}\n"

mainIns :: IO ()
mainIns = do
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
                let frequencia = FrequenciaIns { idFrequenciaIns = idFrequencia, idAlunoIns = idAluno, dataFrequenIns = dataFrequencia, indicPresenIns = indicPresen }
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


