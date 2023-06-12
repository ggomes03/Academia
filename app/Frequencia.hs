module Frequencia (main) where

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


main :: IO ()
main = do
    void initGUI
    
    window <- windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Academia", Gtk.containerBorderWidth Gtk.:= 10]
    window `on` sizeRequest $ return (Requisition 1000 600)

    label <- labelNew (Just "Dados serão exibidos aqui")

    containerAdd window label

    window `on` deleteEvent $ do
        liftIO mainQuit
        return False 
    
    widgetShowAll window

    conn <- open "db/academia.sqlite"


    close conn    

    mainGUI
