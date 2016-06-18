import Graphics.UI.Gtk
import Data.IORef
import qualified Data.Text.IO as T

import Gui
import ScadTreeView
import DSL.Generate

main :: IO()
main = do
    initGUI
    
    builder <- builderNew
    builderAddFromFile builder "data/gui.glade"
    treeStore <- createEmptyTreeStore
    treeView <- builderGetObject builder castToTreeView "treeView"
    menu <- newIORef Nothing

    let gui = Gui builder treeStore treeView menu

    winMain <- builderGetObject builder castToWindow "winMain"
    on winMain objectDestroy mainQuit

    btnExit <- builderGetObject builder castToButton "btnExit"
    on btnExit buttonActivated mainQuit

    btnSave <- builderGetObject builder castToButton "btnSave"
    on btnSave buttonActivated (handleSave gui)

    btnOpen <- builderGetObject builder castToButton "btnOpen"
    on btnOpen buttonActivated (handleOpen gui)

    btnNew <- builderGetObject builder castToButton "btnNew"
    on btnNew buttonActivated (handleNew gui)

    createTreeView gui

    widgetShowAll winMain
    mainGUI

handleSave :: Gui -> IO()
handleSave gui = do
    let builder = _builder gui
    let treeStore = _treeStore gui
    scadTree <- treeStoreGetTree treeStore [0]
    putStrLn "Saving..."
    txtValue <- builderGetObject builder castToEntry "txtValue"
    valueString <- entryGetText txtValue
    putStrLn valueString

    txtName <- builderGetObject builder castToEntry "txtName"
    valueName <- entryGetText txtName
    putStrLn valueName

    T.writeFile "default.tree" $ forestToText [scadTree]

handleOpen :: Gui -> IO()
handleOpen gui = do
    let treeStore = _treeStore gui
    fileContent <- readFile "default.tree"
    let forest = read fileContent
    updateTreeStore treeStore forest

handleNew :: Gui -> IO()
handleNew gui = do
    let treeStore = _treeStore gui
    updateTreeStore treeStore emptyForest
