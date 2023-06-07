module Main where

import Lib
import Control.Applicative  
import Database.SQLite.Simple
import Data.String (fromString)
import Database.SQLite.Simple.FromRow
import Graphics.UI.Gtk hiding (set)
import qualified Graphics.UI.Gtk as Gtk
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Plans

main :: IO ()
main = do
    void initGUI
    window <- windowNew

    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]
    
    box <- vBoxNew False 10 

    label <- labelNew (Just "Dados serão exibidos aqui!")

    containerAdd box label

    buttonPlans <- buttonNewWithLabel "Abrir Planos"
    
    buttonPlans `on` buttonActivated $ Plans.main
    
    widgetShow buttonPlans
    containerAdd box buttonPlans
    
    containerAdd window box 

    window `on` deleteEvent $ do 
        liftIO mainQuit
        return False

    widgetShowAll window
    mainGUI
  
