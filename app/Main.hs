module Main where

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

import Aulas
import Alunos
import Frequencia
import Plano

main :: IO ()
main = do
    void initGUI
    window <- windowNew

    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]
    
    -- window `on` sizeRequest $ return (Requisition 800 600) 
            

    box <- vBoxNew False 10
    Gtk.set box [containerBorderWidth Gtk.:= 10, boxHomogeneous Gtk.:= True, boxSpacing Gtk.:= 10]

    -- Adiciona os Titulo aos botoes
    buttonPlans <- buttonNewWithLabel "Planos"
    widgetSetSizeRequest buttonPlans 200 60

    buttonAlunMat <- buttonNewWithLabel "Alunos"
    widgetSetSizeRequest buttonAlunMat 200 60
    
    buttonFreq <- buttonNewWithLabel "Frequencia"
    widgetSetSizeRequest buttonFreq 200 60

    buttonMinis <- buttonNewWithLabel "Aulas"
    widgetSetSizeRequest buttonMinis 200 60


    -- Adiciona Ação aos botoes
    buttonPlans `on` buttonActivated $ Plano.main
    buttonAlunMat `on` buttonActivated $ Alunos.main
    buttonFreq `on` buttonActivated $ Frequencia.main
    buttonMinis `on` buttonActivated $ Aulas.main
    
    --permite que os botoes sejam visualizados
    widgetShow buttonPlans
    widgetShow buttonAlunMat
    widgetShow buttonFreq
    widgetShow buttonMinis

    --adiciona os botoes no container interno da janela
    containerAdd box buttonPlans
    containerAdd box buttonAlunMat
    containerAdd box buttonFreq
    containerAdd box buttonMinis
    
    --adiciona o container dentro da janela
    containerAdd window box 


    --permite o fim da execução da janela
    window `on` deleteEvent $ do 
        liftIO mainQuit
        return False

    widgetShowAll window
    mainGUI
  
