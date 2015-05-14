import Data.Tree
import Data.Maybe
import Data.IORef

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Control.Monad.IO.Class (liftIO)
import Control.Monad

import ScadDSL


data Gui = Gui
    {
        _builder :: Builder,
        _treeStore :: TreeStore Scad,
        _treeView :: TreeView
    }

main = do
    initGUI

    builder <- builderNew
    builderAddFromFile builder "gui.glade"

    winMain <- builderGetObject builder castToWindow "winMain"
    on winMain objectDestroy mainQuit
    
    btnExit <- builderGetObject builder castToButton "btnExit"
    on btnExit buttonActivated mainQuit

    btnSave <- builderGetObject builder castToButton "btnSave"
    on btnSave buttonActivated (save builder)
    
    let tree = Node  Union [Node (Sphere 1.0) [], Node (Cube 2.0) []]
    let forest = [tree, tree]
    treeStore <- treeStoreNew forest
    
    treeView <- builderGetObject builder castToTreeView "treeView"

    let gui = Gui builder treeStore treeView
    
    createTreeView gui

    widgetShowAll winMain
    mainGUI


save :: Builder -> IO()
save builder = do
    putStrLn "Saving..."
    txtValue <- builderGetObject builder castToEntry "txtValue"
    valueString <- entryGetText txtValue
    putStrLn valueString

    txtName <- builderGetObject builder castToEntry "txtName"
    valueName <- entryGetText txtName
    putStrLn valueName

createTreeView gui = do
    let treeStore = _treeStore gui
    let builder = _builder gui
    let treeView = _treeView gui

    treeViewSetModel treeView treeStore

    col <- treeViewColumnNew
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer False
    cellLayoutSetAttributes col renderer treeStore
           $ \ind -> [cellText := show(ind)]
    treeViewAppendColumn treeView col
    treeSelection <- treeViewGetSelection treeView
    treeSelectionSetMode treeSelection  SelectionSingle
    on treeSelection treeSelectionSelectionChanged (nodeSelected treeStore treeSelection)
    on treeView buttonPressEvent (tryEvent (do button <- eventButton
                                               time <- eventTime
                                               pos <- eventCoordinates
                                               case button of
                                                    RightButton -> liftIO (mouseButtonPressed time pos gui)
                                            ))

--mouseButtonPressed :: TimeStamp -> IO ()
mouseButtonPressed time pos gui = do
    let treeView = _treeView gui
    let (x,y) = pos
    pathInfo <- treeViewGetPathAtPos treeView (floor x, floor y)
    sel <- treeViewGetSelection treeView
    case pathInfo of
        Just (path, _, _) -> do
            treeSelectionSelectPath sel path
            
            menu <- menuNew
            menuItem <- menuItemNewWithLabel "Add 3D object"
            on menuItem menuItemActivated (addNode gui path (Cube 3.0))
            menuShellAppend menu menuItem
            menuItem <- menuItemNewWithLabel "Add Transformations"
            on menuItem menuItemActivated (putStrLn "Transformation added")
            menuShellAppend menu menuItem
            widgetShowAll menu
            menuPopup menu Nothing --(Just (RightButton, time))

addNode gui path node = do
    let treeStore = _treeStore gui
    treeStoreInsert treeStore path 0 node
    putStrLn "dummy"

nodeSelected treeStore treeSelection = do
    maybeTreeIter <- treeSelectionGetSelected treeSelection
    case maybeTreeIter of
        Just treeIter -> do
            treePath <- treeModelGetPath treeStore treeIter
            value <- treeStoreGetValue treeStore treePath
            putStrLn $ "Selected node:" ++ show (value)
    

oneSelection :: ListStore String -> TreeSelection ->  IO ()
oneSelection list tree = do
   sel <- treeSelectionGetSelectedRows tree
   let s = head  (head sel)
   v <- listStoreGetValue list s
   putStrLn $ "selected " ++ v
