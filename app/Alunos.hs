module Alunos (main) where

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

import AlunosInsert


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

-- Função para criar a tabela com os dados
createTable :: [Aluno] -> IO Widget
createTable alunos = do
    -- Cria o TreeView
    treeView <- treeViewNew

    -- Cria as colunas da tabela
    columnId <- treeViewColumnNew
    columnNome <- treeViewColumnNew
    columnDataNasc <- treeViewColumnNew
    columnEmail <- treeViewColumnNew
    columnFone <- treeViewColumnNew

    -- Adiciona as colunas ao TreeView
    treeViewAppendColumn treeView columnId
    treeViewAppendColumn treeView columnNome
    treeViewAppendColumn treeView columnDataNasc
    treeViewAppendColumn treeView columnEmail
    treeViewAppendColumn treeView columnFone

    -- Define os títulos das colunas
    treeViewColumnSetTitle columnId "Id"
    treeViewColumnSetTitle columnNome "Nome"
    treeViewColumnSetTitle columnDataNasc "Data Nascimento"
    treeViewColumnSetTitle columnEmail "Email"
    treeViewColumnSetTitle columnFone "Fone"

    -- Cria o ListStore para armazenar os dados
    store <- listStoreNew alunos

    -- Cria os CellRenderers para exibir os dados nas colunas
    rendererId <- cellRendererTextNew
    rendererNome <- cellRendererTextNew
    rendererDataNasc <- cellRendererTextNew
    rendererEmail <- cellRendererTextNew
    rendererFone <- cellRendererTextNew

    -- Adiciona os CellRenderers às colunas
    treeViewColumnPackStart columnId rendererId True
    treeViewColumnPackStart columnNome rendererNome True
    treeViewColumnPackStart columnDataNasc rendererDataNasc True
    treeViewColumnPackStart columnEmail rendererEmail True
    treeViewColumnPackStart columnFone rendererFone True

    -- Define as propriedades dos CellRenderers
    cellLayoutSetAttributes columnId rendererId store $ \row ->
        [cellText Gtk.:= show (idAluno row)]
    cellLayoutSetAttributes columnNome rendererNome store $ \row ->
        [cellText Gtk.:= nome row]
    cellLayoutSetAttributes columnDataNasc rendererDataNasc store $ \row ->
        [cellText Gtk.:= dataNascimento row]
    cellLayoutSetAttributes columnEmail rendererEmail store $ \row ->
        [cellText Gtk.:= email row]
    cellLayoutSetAttributes columnFone rendererFone store $ \row ->
        [cellText Gtk.:= fone row]

    -- Define o modelo de dados do TreeView
    treeViewSetModel treeView (Just store)

    -- Retorna o TreeView como Widget
    return $ toWidget treeView

main :: IO ()
main = do
    void initGUI
    
    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]
    -- window `on` sizeRequest $ return (Requisition 1000 600)

    buttonInsert <- buttonNewWithLabel "Inserir novo Aluno"

    -- buttonInsert `on` buttonActivated $ AlunosInsert.mainIns

    buttonInsert `on` buttonActivated $ do
        -- widgetDestroy window
        AlunosInsert.mainIns
        
    box <- vBoxNew False 10
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxHomogeneous Gtk.:= True, boxSpacing Gtk.:= 10]

    conn <- open "db/academia.sqlite"
    let query = fromString "SELECT * FROM Alunos" :: Query
    results <- query_ conn query :: IO [Aluno]
    table <- createTable results
    close conn 

    containerAdd box buttonInsert
    containerAdd box table
    containerAdd window box

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False 
    
    
    widgetShowAll window

       

    mainGUI
