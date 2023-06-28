module Tipos where

import Lib
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


data Planos = Planos {idPlano :: Int, nomePlano :: String, descricao :: String, preco :: Float }
instance FromRow Planos where
    fromRow = Planos <$> field <*> field <*> field <*> field



data Frequencia = Frequencia {idFrequencia :: Int, idAlunoFrequen :: Int, dataFrequen :: String, indicPresen :: String }
instance FromRow Frequencia where
    fromRow = Frequencia <$> field <*> field <*> field <*> field



data Aulas = Aulas {idAula :: Int, nomeAula :: String, nomeInstrutor :: String, horarioAula :: Int}
instance FromRow Aulas where
    fromRow = Aulas <$> field <*> field <*> field  <*> field 



data Aluno = Aluno {idAluno :: Int, nome :: String, dataNascimento :: String, email :: String, fone :: String}
instance FromRow Aluno where
    fromRow = Aluno <$> field <*> field <*> field  <*> field <*> field
