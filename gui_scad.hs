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
    
    let tree = Node Root [Node (Sphere 1.0) [], Node (Cube 2.0) []]
    let forest = [tree]
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
    let treeStore = _treeStore gui
    let menuIORef = _menu gui
    let (x,y) = pos
    pathInfo <- treeViewGetPathAtPos treeView (floor x, floor y)
    sel <- treeViewGetSelection treeView
    case pathInfo of
        Just (path, _, _) -> do
            treeSelectionSelectPath sel path 
            node <- treeStoreGetValue treeStore path
            menu <- menuNew
            writeIORef menuIORef (Just menu)

            let add3dObjectAllowed = isMenuActionAllowed Add3Object node
            subMenu <- createSubMenu menu "Add 3D Object" add3dObjectAllowed
            addMenuItem subMenu "Sphere" True (addNode gui path (Sphere 1.0))
            addMenuItem subMenu "Cube" True (addNode gui path (Cube 1.0))
            addMenuItem subMenu "Cylinder" True (addNode gui path (Cylinder 1.0 1.0 True))

            let addTransformationAllowed = isMenuActionAllowed AddTransformation node
            subMenu <- createSubMenu menu "Add Transformation" addTransformationAllowed
            addMenuItem subMenu "Translate" True (addNode gui path (Translate 0 0 0))
            addMenuItem subMenu "Rotate" True (addNode gui path (Rotate 0 0 0))
            
            let addBooleanAllowed = isMenuActionAllowed AddBooleanOperation node
            subMenu <- createSubMenu menu "Add Boolean Operation" addBooleanAllowed
            addMenuItem subMenu "Union" True (addNode gui path (Union))
            addMenuItem subMenu "Difference" True (addNode gui path (Difference)) 
            addMenuItem subMenu "Intersection" True (addNode gui path (Intersection))

            let deleteAllowed = isMenuActionAllowed DeleteNode node
            addMenuItem menu "Delete Node" deleteAllowed (deleteNode gui path)

            widgetShowAll menu
            menuPopup menu (Just (RightButton, time))
        otherwise -> return ()

data MenuAction = Add3Object | AddTransformation | AddBooleanOperation | DeleteNode


isMenuActionAllowed :: MenuAction -> Scad -> Bool
isMenuActionAllowed DeleteNode Root = False
isMenuActionAllowed DeleteNode _ = True
isMenuActionAllowed _ (Sphere _) = False 
isMenuActionAllowed _ (Cube _) = False 
isMenuActionAllowed _ (Cylinder _ _ _) = False 
isMenuActionAllowed _ _ = True


createSubMenu parentMenu label enabled = do
    menuItem <- menuItemNewWithLabel label
    menuShellAppend parentMenu menuItem
    subMenu <- menuNew
    case enabled of
        True -> do
            menuItemSetSubmenu menuItem subMenu
        False -> do
            disableChildWidget menuItem
            
    return subMenu
            

addMenuItem menu label enabled callback = do
    menuItem <- menuItemNewWithLabel label
    menuShellAppend menu menuItem
    case enabled of
        True -> do
            on menuItem menuItemActivated callback
            return ()
        False -> do
            disableChildWidget menuItem

disableChildWidget bin = do
    maybeWidget  <- binGetChild bin
    case maybeWidget of
        Just widget -> do       
            widgetSetSensitive widget False

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
        otherwise -> return ()
