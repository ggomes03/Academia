module AlunosInsert where

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

import Tipos

mainIns :: IO ()
mainIns = do
    void initGUI

    window <- windowNew
    Gtk.set window [ windowTitle Gtk.:= "Academia", containerBorderWidth Gtk.:= 10 ]

    -- Criar widgets
    idAlunoEntry <- entryNew
    nomeEntry <- entryNew
    dataNascimentoEntry <- entryNew
    emailEntry <- entryNew
    foneEntry <- entryNew
    inserirButton <- buttonNewWithLabel "Inserir"

    -- Criar layout
    vbox <- vBoxNew False 10
    containerAdd window vbox
    table <- tableNew 5 2 False
    boxPackStart vbox table PackGrow 0
    boxPackStart vbox inserirButton PackNatural 0

    -- Adicionar labels e entries à tabela
    addLabelAndEntry table 0 "ID:" idAlunoEntry
    addLabelAndEntry table 1 "Nome:" nomeEntry
    addLabelAndEntry table 2 "Data de Nascimento:" dataNascimentoEntry
    addLabelAndEntry table 3 "Email:" emailEntry
    addLabelAndEntry table 4 "Telefone:" foneEntry

    inserirButton `on` buttonActivated $ do
        idAluno <- entryGetText idAlunoEntry >>= return . read
        nome <- entryGetText nomeEntry >>= return . fromString
        dataNascimento <- entryGetText dataNascimentoEntry >>= return . fromString
        email <- entryGetText emailEntry >>= return . fromString
        fone <- entryGetText foneEntry >>= return . fromString
        let aluno = Aluno { idAluno = idAluno, nome = nome, dataNascimento = dataNascimento, email = email, fone = fone }
        conn <- open "db/academia.sqlite"
        let query = fromString "INSERT INTO Alunos (idAluno, nome, dataNascimento, email, fone) VALUES (?, ?, ?, ?, ?)" :: Query
        execute conn query (idAluno, nome, dataNascimento, email, fone)
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