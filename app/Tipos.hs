module Tipos where

import Lib
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


data Planos = Planos {idPlano :: Int, nomePlano :: String, descricao :: String, preco :: Float }
instance FromRow Planos where
    fromRow = Planos <$> field <*> field <*> field <*> field
instance Show Planos where
    show (Planos idPlano nomePlano descricao preco) =
        "Plano {Id = " ++ show idPlano ++ 
        ", nome = " ++ show nomePlano ++
        ", descricao = " ++ show descricao ++
        ", preco = " ++ show preco ++ "}\n"



data Frequencia = Frequencia {idFrequencia :: Int, idAlunoFrequen :: Int, dataFrequen :: String, indicPresen :: String }
instance FromRow Frequencia where
    fromRow = Frequencia <$> field <*> field <*> field <*> field
instance Show Frequencia where
    show (Frequencia idFrequencia idAluno dataFrequen indicPresen) =
        "Plano {Id = " ++ show idFrequencia ++ 
        ", nome = " ++ show idAluno ++
        ", descricao = " ++ show dataFrequen ++
        ", preco = " ++ show indicPresen ++ "}\n"



data Aulas = Aulas {idAula :: Int, nomeAula :: String, nomeInstrutor :: String, horarioAula :: Int}
instance FromRow Aulas where
    fromRow = Aulas <$> field <*> field <*> field  <*> field 
instance Show Aulas where
    show (Aulas idAula nomeAula nomeInstrutor horarioAula) =
        "Aluno {Id = " ++ show idAula ++
        ", Aula = " ++ show nomeAula ++
        ", Instrutor = " ++ show nomeInstrutor ++
        ", Hora = " ++ show horarioAula ++ "}\n"



data Aluno = Aluno {idAluno :: Int, nome :: String, dataNascimento :: String, email :: String, fone :: String}
instance FromRow Aluno where
    fromRow = Aluno <$> field <*> field <*> field  <*> field <*> field
instance Show Aluno where
    show (Aluno idAluno nome dataNascimento email fone) =
        "Aluno {Id = " ++ show idAluno ++
        ", nome = " ++ show nome ++
        ", dataNascimento = " ++ show dataNascimento ++
        ", email = " ++ show email ++
        ", fone = " ++ show fone ++ "}\n"