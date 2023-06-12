module Aulas (main) where

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

data Aulas = Aulas {idAula :: Int, nomeAula :: String, nomeInstrutor :: String, horarioAula :: String}

instance FromRow Aulas where
    fromRow = Aulas <$> field <*> field <*> field  <*> field 

instance Show Aulas where
    show (Aulas idAula nomeAula nomeInstrutor horarioAula) =
        "Aluno {Id = " ++ show idAula ++
        ", nome = " ++ show nomeAula ++
        ", dataNascimento = " ++ show nomeInstrutor ++
        ", email = " ++ show horarioAula ++ "}\n"

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

main :: IO ()
main = do
    void initGUI

    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]

    buttonInsert <- buttonNewWithLabel "Inserir nova Aula"

    box <- vBoxNew False 10
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxHomogeneous Gtk.:= True, boxSpacing Gtk.:= 10]

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

    -- widgetShow buttonInsert
    widgetShowAll window
    mainGUI