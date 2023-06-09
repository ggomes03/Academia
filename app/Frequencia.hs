module Frequencia (main) where

import Lib
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Data.String (fromString)
import qualified Data.Text as T
import Data.Char (toUpper)
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


import Tipos


-- Função para criar a tabela com os dados
createTable :: [Frequencia] -> IO Widget
createTable frequencia = do
    -- Cria o TreeView
    treeView <- treeViewNew

    -- Cria as colunas da tabela
    columnIdFrequencia <- treeViewColumnNew
    columnIdAluno <- treeViewColumnNew
    columnNomeAluno <- treeViewColumnNew
    columnDataFrequencia <- treeViewColumnNew
    columnIndicPresen <- treeViewColumnNew

    -- Adiciona as colunas ao TreeView
    treeViewAppendColumn treeView columnIdFrequencia
    treeViewAppendColumn treeView columnIdAluno
    treeViewAppendColumn treeView columnNomeAluno
    treeViewAppendColumn treeView columnDataFrequencia
    treeViewAppendColumn treeView columnIndicPresen

    -- Define os títulos das colunas
    treeViewColumnSetTitle columnIdFrequencia "Id Frequencia"
    treeViewColumnSetTitle columnIdAluno "Id Aluno"
    treeViewColumnSetTitle columnNomeAluno "Nome Aluno"
    treeViewColumnSetTitle columnDataFrequencia "Data"
    treeViewColumnSetTitle columnIndicPresen "Situação"

    -- Cria o ListStore para armazenar os dados
    store <- listStoreNew frequencia

    -- Cria os CellRenderers para exibir os dados nas colunas
    rendererIdFrequencia <- cellRendererTextNew
    rendererIdAluno <- cellRendererTextNew
    rendererNomeAluno <- cellRendererTextNew
    rendererDataFrequencia <- cellRendererTextNew
    rendererIndicPresen <- cellRendererTextNew

    -- Adiciona os CellRenderers às colunas
    treeViewColumnPackStart columnIdFrequencia rendererIdFrequencia True
    treeViewColumnPackStart columnIdAluno rendererIdAluno True
    treeViewColumnPackStart columnNomeAluno rendererNomeAluno True
    treeViewColumnPackStart columnDataFrequencia rendererDataFrequencia True
    treeViewColumnPackStart columnIndicPresen rendererIndicPresen True

    -- Define as propriedades dos CellRenderers
    cellLayoutSetAttributes columnIdFrequencia rendererIdFrequencia store $ \row ->
        [cellText Gtk.:= show (idFrequencia row)]
    cellLayoutSetAttributes columnIdAluno rendererIdAluno store $ \row ->
        [cellText Gtk.:= show (idAlunoFrequen row)]
    cellLayoutSetAttributes columnNomeAluno rendererNomeAluno store $ \row ->
        [cellText Gtk.:= show (nomeAluno row)]
    cellLayoutSetAttributes columnDataFrequencia rendererDataFrequencia store $ \row ->
        [cellText Gtk.:= dataFrequen row]
    cellLayoutSetAttributes columnIndicPresen rendererIndicPresen store $ \row ->
        [cellText Gtk.:= indicPresen row]

    -- Define o modelo de dados do TreeView
    treeViewSetModel treeView (Just store)

    -- Retorna o TreeView como Widget
    return $ toWidget treeView

main :: IO ()
main = do
    void initGUI
    runApp
    

runApp :: IO ()
runApp = do 

    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Frequencias", Gtk.containerBorderWidth Gtk.:= 10]

    buttonInsert <- buttonNewWithLabel "Inserir nova frequencia"


    verificarButton <- buttonNewWithLabel "Verificar Presença"
    verificarAusenButton <- buttonNewWithLabel "Verificar Ausência"

    table <- tableNew 5 2 False
    
    box <- vBoxNew False 10
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxSpacing Gtk.:= 10]

    boxPackStart box table PackGrow 0
    boxPackStart box buttonInsert PackNatural 0
    boxPackStart box verificarButton PackNatural 0
    boxPackStart box verificarAusenButton PackNatural 0

    -- Cria a tabela
    conn <- open "db/academia.sqlite"
    let query = fromString "SELECT Frequencia.idFrequencia, Frequencia.idAluno, Alunos.nome, Frequencia.dataFreq, Frequencia.indicPresen FROM Frequencia, Alunos WHERE Alunos.idAluno = Frequencia.idAluno" :: Query
    results <- query_ conn query :: IO [Frequencia]
    table <- createTable results
    close conn

    -- Adiciona a tabela e o botao de insert à box
    containerAdd box table
    -- containerAdd box buttonInsert

    containerAdd window box 
    
    verificarButton `on` buttonActivated $ do 
        widgetDestroy window
        verificarPresen
    
    verificarAusenButton `on` buttonActivated $ do 
        widgetDestroy window
        verificarAusen

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    buttonInsert `on` buttonActivated $ do 
        widgetDestroy window
        mainInsert

        
    widgetShowAll window
    mainGUI

changeUpper :: String -> String
changeUpper = map toUpper

mainInsert :: IO ()
mainInsert = do
    void initGUI

    window <- windowNew
    Gtk.set window [ windowTitle Gtk.:= "Inserir Frequencia", containerBorderWidth Gtk.:= 10 ]

    -- Criar widgets
    idAlunoEntry <- entryNew
    dataFrequenciaEntry <- entryNew
    indicPresenEntry <- entryNew
    inserirButton <- buttonNewWithLabel "Inserir"
    backButton <- buttonNewWithLabel "Voltar"


    -- Criar layout
    vbox <- vBoxNew False 10
    containerAdd window vbox
    table <- tableNew 5 2 False
    boxPackStart vbox table PackGrow 0
    boxPackStart vbox inserirButton PackNatural 0
    boxPackStart vbox backButton PackNatural 0


    -- Adicionar labels e entries à tabela
    addLabelAndEntry table 1 "ID Aluno:" idAlunoEntry
    addLabelAndEntry table 2 "Data Frequencia:" dataFrequenciaEntry
    addLabelAndEntry table 3 "Presente: S ou N" indicPresenEntry
    
    inserirButton `on` buttonActivated $ do
        idAlunoStr <- entryGetText idAlunoEntry
        dataFrequencia <- entryGetText dataFrequenciaEntry >>= return . fromString
        indicPresen <- entryGetText indicPresenEntry >>= return . changeUpper

        let tamanhoData = length dataFrequencia

        let maybeIdAluno = readMaybe idAlunoStr :: Maybe Int

        if isJust maybeIdAluno
            then do 
                if tamanhoData /= 8 
                    then do 
                        dialog <- messageDialogNew Nothing [] MessageInfo ButtonsClose "erro: Data inválida, formato da data DDMMAAAA!"
                        dialogRun dialog
                        widgetDestroy dialog
                    else do 
                        if indicPresen `notElem` ["N","S"] 
                            then do 
                                dialog <- messageDialogNew Nothing [] MessageInfo ButtonsClose "erro: Caractere de Presença inválido!"
                                dialogRun dialog
                                widgetDestroy dialog
                            else do 

                                let idAluno = fromJust maybeIdAluno
                                    frequencia = Frequencia { idFrequencia = 0, idAlunoFrequen = idAluno, dataFrequen = dataFrequencia, indicPresen = indicPresen }
                                conn <- open "db/academia.sqlite"
                                let queryAux = fromString "SELECT idAluno FROM Alunos" :: Query
                                resultsAlunos <- query_ conn queryAux :: IO [AlunoAux]
                                let idAlunos = map idAlunoAux resultsAlunos :: [Int] 
                                if read idAlunoStr `notElem` idAlunos
                                    then do 
                                        dialog <- messageDialogNew Nothing [] MessageInfo ButtonsClose "erro: Id Inválido!"
                                        dialogRun dialog
                                        widgetDestroy dialog
                                    else do 
                                        let query = fromString "INSERT INTO Frequencia (idAluno, dataFreq, indicPresen) VALUES ( ?, ?, ?)" :: Query
                                        execute conn query (idAluno, dataFrequencia, indicPresen)
                                        close conn
                                        putStrLn "Inserido com sucesso!"

                                        dialog <- messageDialogNew Nothing [] MessageInfo ButtonsClose "Frequencia Inserida!"
                                        dialogRun dialog
                                        widgetDestroy dialog
            else do
                dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal, Gtk.DialogDestroyWithParent] Gtk.MessageError Gtk.ButtonsOk "Erro: Campos Vazios"
                Gtk.dialogRun dialog
                Gtk.widgetDestroy dialog


    -- Configurar ação do fechamento da janela
    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    backButton `on` buttonActivated $ do
		widgetDestroy window
		liftIO mainQuit
		main

    

    widgetShowAll window
    mainGUI

verificarPresen :: IO ()
verificarPresen = do 

    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Frequencias", Gtk.containerBorderWidth Gtk.:= 10]

    box <- vBoxNew False 10
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxSpacing Gtk.:= 10]

    verificarButton <- buttonNewWithLabel "Verificar Presença"
    backButton <- buttonNewWithLabel "Voltar"

    idAlunoEntry <- entryNew
    dataFrequenciaEntry <- entryNew

    table <- tableNew 5 2 False

    addLabelAndEntry table 1 "ID Aluno:" idAlunoEntry
    addLabelAndEntry table 2 "Data:" dataFrequenciaEntry

    boxPackStart box table PackGrow 0
    boxPackStart box verificarButton PackNatural 0
    boxPackStart box backButton PackNatural 0

    tableFiltro <- createTable []
    containerAdd box tableFiltro

    verificarButton `on` buttonActivated $ do 
        idAlunotxt <- entryGetText idAlunoEntry 
        dataFrequenciatxt <- entryGetText dataFrequenciaEntry 

        if null idAlunotxt || null dataFrequenciatxt
            then do 
                dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal, Gtk.DialogDestroyWithParent] Gtk.MessageError Gtk.ButtonsOk "Erro: Campos Vazios"
                Gtk.dialogRun dialog
                Gtk.widgetDestroy dialog
            else do 
                -- Cria a tabela
                conn <- open "db/academia.sqlite"
                -- let query = fromString ("SELECT * FROM Frequencia WHERE idAluno = " ++ idAlunotxt ++ " AND dataFreq = '" ++ dataFrequenciatxt ++ "'") :: Query
                let query = fromString ("SELECT Frequencia.idFrequencia, Frequencia.idAluno, Alunos.nome, Frequencia.dataFreq, Frequencia.indicPresen FROM Frequencia, Alunos WHERE Alunos.idAluno = Frequencia.idAluno AND dataFreq = '" ++ dataFrequenciatxt ++ "' AND Frequencia.idAluno = '" ++ idAlunotxt ++ "'") :: Query
                resultsFiltro <- query_ conn query :: IO [Frequencia]
                tableFiltro <- createTable resultsFiltro
                close conn

                if null resultsFiltro 
                    then do
                        dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal, Gtk.DialogDestroyWithParent] Gtk.MessageError Gtk.ButtonsOk "O aluno não frequentou nesta data! "
                        Gtk.dialogRun dialog
                        Gtk.widgetDestroy dialog
                    else do
                        -- containerRemove box tableFiltro
                        containerForeach box $ \childWidget -> containerRemove box childWidget

                        boxPackStart box table PackGrow 0
                        boxPackStart box verificarButton PackNatural 0
                        boxPackStart box backButton PackNatural 0

                        tableFiltro <- createTable resultsFiltro
                        containerAdd box tableFiltro
                        widgetShowAll tableFiltro

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    backButton `on` buttonActivated $ do
		widgetDestroy window
		main



    containerAdd window box 

    widgetShowAll window
    mainGUI


verificarAusen :: IO ()
verificarAusen = do 

    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Ausências", Gtk.containerBorderWidth Gtk.:= 10]

    box <- vBoxNew False 10
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxSpacing Gtk.:= 10]

    verificarButton <- buttonNewWithLabel "Verificar Ausência"
    backButton <- buttonNewWithLabel "Voltar"

    dataFrequenciaEntry <- entryNew

    table <- tableNew 5 2 False

    addLabelAndEntry table 2 "Data:" dataFrequenciaEntry

    boxPackStart box table PackGrow 0
    boxPackStart box verificarButton PackNatural 0
    boxPackStart box backButton PackNatural 0

    tableFiltro <- createTable []
    containerAdd box tableFiltro

    verificarButton `on` buttonActivated $ do 
        dataFrequenciatxt <- entryGetText dataFrequenciaEntry 

        if null dataFrequenciatxt
            then do 
                dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal, Gtk.DialogDestroyWithParent] Gtk.MessageError Gtk.ButtonsOk "Erro: Data Vazia"
                Gtk.dialogRun dialog
                Gtk.widgetDestroy dialog
            else do 
                -- Cria a tabela
                conn <- open "db/academia.sqlite"
                -- let query = fromString ("SELECT  FROM Frequencia WHERE indicPresen = 'N' AND dataFreq = '" ++ dataFrequenciatxt ++ "'") :: Query
                let query = fromString ("SELECT Frequencia.idFrequencia, Frequencia.idAluno, Alunos.nome, Frequencia.dataFreq, Frequencia.indicPresen FROM Frequencia, Alunos WHERE Alunos.idAluno = Frequencia.idAluno AND Frequencia.indicPresen = 'N' AND dataFreq = '" ++ dataFrequenciatxt ++ "'") :: Query
                resultsFiltro <- query_ conn query :: IO [Frequencia]
                tableFiltro <- createTable resultsFiltro
                close conn

                if null resultsFiltro
                    then do 
                        dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal, Gtk.DialogDestroyWithParent] Gtk.MessageError Gtk.ButtonsOk "Data sem Ausências"
                        Gtk.dialogRun dialog
                        Gtk.widgetDestroy dialog
                    else do 
                        -- containerRemove box tableFiltro
                        containerForeach box $ \childWidget -> containerRemove box childWidget

                        boxPackStart box table PackGrow 0
                        boxPackStart box verificarButton PackNatural 0
                        boxPackStart box backButton PackNatural 0

                        tableFiltro <- createTable resultsFiltro
                        containerAdd box tableFiltro
                        widgetShowAll tableFiltro
               

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    backButton `on` buttonActivated $ do
		widgetDestroy window
		main

    containerAdd window box 

    widgetShowAll window
    mainGUI