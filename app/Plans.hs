module Plans (main) where

import Lib
import Control.Applicative  
import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow
import Graphics.UI.Gtk hiding (set)
import qualified Graphics.UI.Gtk as Gtk
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

data Aluno = Aluno {idAluno :: Int, nome :: String, dataNascimento :: String, email :: String, fone :: String}

instance FromRow Aluno where
    fromRow = Aluno <$> field <*> field <*> field  <*> field <*> field

instance Show Aluno where
    show (Aluno idAluno nome dataNascimento email fone) =
        "\nAluno {idAluno = " ++ show idAluno ++
        ", nome = " ++ show nome ++
        ", dataNascimento = " ++ show dataNascimento ++
        ", email = " ++ show email ++
        ", fone = " ++ show fone ++ "}"

data Planos = Planos {idPlano :: Int, nomePlano :: String, descricao :: String, preco :: Float}

instance FromRow Planos where
    fromRow = Planos <$> field <*> field <*> field <*> field

instance Show Planos where
    show (Planos idPlano nomePlano descricao preco) = 
        "\nPlano {idPlano = " ++ show idPlano ++
        ", nome = " ++ show nomePlano ++
        ", descricao = " ++ show descricao ++
        ", preco = " ++ show preco ++ "}"

main :: IO ()
main = do 

    void initGUI

    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]

    label <- labelNew (Just "Dados serao exibidos aqui")

    containerAdd window label 

    window `on` deleteEvent $ do 
        liftIO mainQuit 
        return False 

    widgetShowAll window

    conn <- open "db/academia.sqlite"

    let query = fromString "SELECT * FROM Alunos" :: Query
    results <- query_ conn query :: IO [Aluno]
    labelSetText label (show results)

    let querydois = fromString "SELECT * FROM Planos" :: Query
    resultsdois <- query_ conn querydois :: IO [Planos]
    labelSetText label(show resultsdois)

    close conn

    mainGUI

    
