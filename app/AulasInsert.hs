module AulasInsert where

import Lib
import Control.Applicative  
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.String (fromString)
import Graphics.UI.Gtk hiding (set)
import qualified Graphics.UI.Gtk as Gtk
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Monad (void)

data AulasInsert = AulasInsert { idAulaIns :: Int, nomeAulaIns :: String, nomeInstrutorIns :: String, horarioAulaIns :: Int }

instance FromRow AulasInsert where
    fromRow = AulasInsert <$> field <*> field <*> field  <*> field

instance Show AulasInsert where
    show (AulasInsert idAulaIns nomeAulaIns nomeInstrutorIns horarioAulaIns) =
        "Aula {Id = " ++ show idAulaIns ++
        ", nome = " ++ show nomeAulaIns ++
        ", Instrutor = " ++ show nomeInstrutorIns ++
        ", Hora = " ++ show horarioAulaIns ++
        "}\n"

mainIns :: IO ()
mainIns = do
    void initGUI

    window <- windowNew
    Gtk.set window [ windowTitle Gtk.:= "Academia", containerBorderWidth Gtk.:= 10 ]

    -- Criar widgets
    idAulaEntry <- entryNew
    nomeEntry <- entryNew
    nomeInstrutorEntry <- entryNew
    horarioAulaEntry <- entryNew
    inserirButton <- buttonNewWithLabel "Inserir"

    -- Criar layout
    vbox <- vBoxNew False 10
    containerAdd window vbox
    table <- tableNew 5 2 False
    boxPackStart vbox table PackGrow 0
    boxPackStart vbox inserirButton PackNatural 0

    -- Adicionar labels e entries à tabela
    addLabelAndEntry table 0 "ID:" idAulaEntry
    addLabelAndEntry table 1 "Nome:" nomeEntry
    addLabelAndEntry table 2 "Instrutor:" nomeInstrutorEntry
    addLabelAndEntry table 3 "Hora:" horarioAulaEntry

    -- inserirButton `on` buttonActivated $ do
    --     idAula <- entryGetText idAulaEntry >>= return . read
    --     nomeAula <- entryGetText nomeEntry >>= return . fromString
    --     nomeInstrutor <- entryGetText nomeInstrutorEntry >>= return . fromString
    --     horarioAula <- entryGetText horarioAulaEntry >>= return . read
    --     let aula = AulasInsert { idAulaIns = idAula, nomeAulaIns = nomeAula, nomeInstrutorIns = nomeInstrutor, horarioAulaIns = horarioAula }
    --     conn <- open "db/academia.sqlite"
    --     let query = fromString "INSERT INTO Aulas (idAula, nomeAula, nomeInstrutor, horarioAula) VALUES (?, ?, ?, ?)" :: Query
    --     execute conn query (idAula, nomeAula, nomeInstrutor, horarioAula)
    --     close conn
    --     putStrLn "Inserido com sucesso!"

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
                let aula = AulasInsert { idAulaIns = idAula, nomeAulaIns = nomeAula, nomeInstrutorIns = nomeInstrutor, horarioAulaIns = horarioAula }
                conn <- open "db/academia.sqlite"
                let query = fromString "INSERT INTO Aulas (idAula, nomeAula, nomeInstrutor, horarioAula) VALUES (?, ?, ?, ?)" :: Query
                execute conn query (idAula, nomeAula, nomeInstrutor, horarioAula)
                close conn
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