module Tipos where

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
import Data.Maybe (isJust, fromJust)
import System.IO.Error (ioeGetErrorString)

data Planos = Planos {idPlano :: Int, nomePlano :: String, descricao :: String, preco :: Float }
instance FromRow Planos where
    fromRow = Planos <$> field <*> field <*> field <*> field



data Frequencia = Frequencia {idFrequencia :: Int, idAlunoFrequen :: Int, nomeAluno :: String, dataFrequen :: String, indicPresen :: String }
instance FromRow Frequencia where
    fromRow = Frequencia <$> field <*> field <*> field <*> field <*> field



data Aulas = Aulas {idAula :: Int, nomeAula :: String, nomeInstrutor :: String, horarioAula :: Int}
instance FromRow Aulas where
    fromRow = Aulas <$> field <*> field <*> field  <*> field 



data Aluno = Aluno {idAluno :: Int, nome :: String, dataNascimento :: String, email :: String, fone :: String}
instance FromRow Aluno where
    fromRow = Aluno <$> field <*> field <*> field  <*> field <*> field


-- Função auxiliar para adicionar uma label e uma entry a uma tabela
addLabelAndEntry :: Table -> Int -> String -> Entry -> IO ()
addLabelAndEntry table row label entry = do
    labelWidget <- labelNew (Just label)
    miscSetAlignment labelWidget 0 0.5
    tableAttachDefaults table labelWidget 0 1 row (row + 1)
    tableAttachDefaults table entry 1 2 row (row + 1)
