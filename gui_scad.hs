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
        _treeView :: TreeView,
        _menu :: IORef (Maybe Menu) -- Hack to make sure that the GC(?) will not remove the menu when used
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
    menu <- newIORef Nothing

    let gui = Gui builder treeStore treeView menu
    
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
    let menuIORef = _menu gui
    let (x,y) = pos
    pathInfo <- treeViewGetPathAtPos treeView (floor x, floor y)
    sel <- treeViewGetSelection treeView
    case pathInfo of
        Just (path, _, _) -> do
            treeSelectionSelectPath sel path 
            menu <- menuNew
            writeIORef menuIORef (Just menu)

            subMenu <- createSubMenu menu "Add 3D Object"
            addMenuItem subMenu "Sphere" (addNode gui path (Sphere 1.0))
            addMenuItem subMenu "Cube" (addNode gui path (Cube 1.0))
            addMenuItem subMenu "Cylinder" (addNode gui path (Cylinder 1.0 1.0 True))

            subMenu <- createSubMenu menu "Add Transformation"
            addMenuItem subMenu "Translate" (addNode gui path (Translate 0 0 0))
            addMenuItem subMenu "Rotate" (addNode gui path (Rotate 0 0 0))
            
            subMenu <- createSubMenu menu "Add Boolean Operation"
            addMenuItem subMenu "Union" (addNode gui path (Union))
            addMenuItem subMenu "Difference" (addNode gui path (Difference))
            addMenuItem subMenu "Intersection" (addNode gui path (Intersection))

            addMenuItem menu "Delete Node" (deleteNode gui path)
            
            widgetShowAll menu
            menuPopup menu (Just (RightButton, time))



createSubMenu parentMenu label = do
    menuItem <- menuItemNewWithLabel label
    menuShellAppend parentMenu menuItem
    subMenu <- menuNew
    menuItemSetSubmenu menuItem subMenu
    return subMenu

addMenuItem menu label callback = do
    menuItem <- menuItemNewWithLabel label
    on menuItem menuItemActivated callback
    menuShellAppend menu menuItem

addNode gui path node = do
    let treeStore = _treeStore gui
    treeStoreInsert treeStore path 0 node

deleteNode gui path = do
    let treeStore = _treeStore gui
    treeStoreRemove treeStore path
    return () 

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
