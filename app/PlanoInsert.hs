module PlanoInsert where

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

import Tipos

mainIns :: IO ()
mainIns = do
    void initGUI

    window <- windowNew
    Gtk.set window [ windowTitle Gtk.:= "Academia", containerBorderWidth Gtk.:= 10 ]

    -- Criar widgets
    idPlanoEntry <- entryNew
    nomePlanoEntry <- entryNew
    descricaoEntry <- entryNew
    precoEntry <- entryNew
    inserirButton <- buttonNewWithLabel "Inserir"

    -- Criar layout
    vbox <- vBoxNew False 10
    containerAdd window vbox
    table <- tableNew 5 2 False
    boxPackStart vbox table PackGrow 0
    boxPackStart vbox inserirButton PackNatural 0

    -- Adicionar labels e entries à tabela
    addLabelAndEntry table 0 "ID:" idPlanoEntry
    addLabelAndEntry table 1 "Nome:" nomePlanoEntry
    addLabelAndEntry table 2 "Descrição:" descricaoEntry
    addLabelAndEntry table 3 "Preço:" precoEntry

    inserirButton `on` buttonActivated $ do
        idPlano <- entryGetText idPlanoEntry >>= return . read
        nomePlano <- entryGetText nomePlanoEntry >>= return . fromString
        descricao <- entryGetText descricaoEntry >>= return . fromString
        preco <- entryGetText precoEntry >>= return . read
        let plano = Planos { idPlano = idPlano, nomePlano = nomePlano, descricao = descricao, preco = preco }
        conn <- open "db/academia.sqlite"
        let query = fromString "INSERT INTO Planos (idPlano, nome, descricao, preco) VALUES (?, ?, ?, ?)" :: Query
        execute conn query (idPlano, nomePlano, descricao, preco)
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


