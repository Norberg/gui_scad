module ScadTreeView(
    createEmptyTreeStore,
    createTreeView
)
where 

import Data.Tree
import Data.IORef
import Data.List

import Graphics.UI.Gtk

import Control.Monad.IO.Class (liftIO)

import ScadDSL
import Gui

data MenuAction = Add3Object 
                | AddTransformation
                | AddBooleanOperation 
                | DeleteNode
                | Move


isMenuActionAllowed :: MenuAction -> Scad -> Bool
isMenuActionAllowed DeleteNode Root = False
isMenuActionAllowed DeleteNode _ = True
isMenuActionAllowed _ (Sphere _) = False 
isMenuActionAllowed _ (Cube _) = False 
isMenuActionAllowed _ (Cylinder _ _ _) = False 
isMenuActionAllowed _ _ = True

isScadNodeMovable :: Scad -> Bool
isScadNodeMovable Root = False
isScadNodeMovable _ = True

createEmptyTreeStore :: IO (TreeStore Scad)
createEmptyTreeStore = do
    let tree = Node Root [Node (Sphere 1.0) [], Node (Cube 2.0) []]
    let forest = [tree]
    treeStore <- treeStoreNewDND forest 
                        (Just treeStoreDragSourceIface)
                        (Just treeStoreDragDestIface) 
    return treeStore    

createTreeView :: Gui -> IO(ConnectId TreeView)
createTreeView gui = do
    let treeStore = _treeStore gui
    let treeView = _treeView gui

    treeViewSetModel treeView treeStore
    treeViewSetReorderable treeView True
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
                                                    _ -> stopEvent
                                            ))

createSubMenu :: Menu -> String -> Bool -> IO(Menu)
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
            
addMenuItem :: Menu -> String -> Bool -> IO() -> IO()
addMenuItem menu label enabled callback = do
    menuItem <- menuItemNewWithLabel label
    menuShellAppend menu menuItem
    case enabled of
        True -> do
            on menuItem menuItemActivated callback
            return ()
        False -> do
            disableChildWidget menuItem

disableChildWidget :: BinClass bin => bin -> IO()
disableChildWidget bin = do
    maybeWidget  <- binGetChild bin
    case maybeWidget of
        Just widget -> do       
            widgetSetSensitive widget False
        Nothing -> return ()

addNode :: Gui -> TreePath -> Scad -> IO()
addNode gui path node = do
    let treeStore = _treeStore gui
    treeStoreInsert treeStore path 0 node

deleteNode :: Gui -> TreePath -> IO()
deleteNode gui path = do
    let treeStore = _treeStore gui
    treeStoreRemove treeStore path
    return () 

mouseButtonPressed :: TimeStamp -> (Double, Double) -> Gui -> IO()
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
        Nothing -> return ()


nodeSelected :: TreeStore Scad -> TreeSelection -> IO()
nodeSelected treeStore treeSelection = do
    maybeTreeIter <- treeSelectionGetSelected treeSelection
    case maybeTreeIter of
        Just treeIter -> do
            treePath <- treeModelGetPath treeStore treeIter
            value <- treeStoreGetValue treeStore treePath
            putStrLn $ "Selected node:" ++ show (value)
        Nothing -> return ()


treeStoreDragSourceIface :: DragSourceIface TreeStore Scad
treeStoreDragSourceIface = DragSourceIface {
    treeDragSourceRowDraggable = \treeStore src -> do
        node <- treeStoreGetValue treeStore src
        return (isScadNodeMovable node),
    treeDragSourceDragDataGet = treeSetRowDragData,
    treeDragSourceDragDataDelete = \model dest@(_:_) -> do
            liftIO $ treeStoreRemove model dest
            return True

  }

treeStoreDragDestIface :: DragDestIface TreeStore Scad
treeStoreDragDestIface = DragDestIface {
    treeDragDestRowDropPossible = \model dest -> do
        mModelPath <- treeGetRowDragData
        case mModelPath of
            Nothing -> return False
            Just (model', source) -> 
                if (toTreeModel model/=toTreeModel model') then return False
                else liftIO $ do
                    case (init dest) of
                        (_:_) -> do
                            if (isPrefixOf source dest) then return False
                            else do
                                valueParrentDest <- treeStoreGetValue model (init dest)
                                return (isMenuActionAllowed Move valueParrentDest)
                        [] -> return False,
    treeDragDestDragDataReceived = \model dest@(_:_) -> do
      mModelPath <- treeGetRowDragData
      case mModelPath of
        Nothing -> return False
        Just (_, []) -> return False
        Just (model', source@(_:_)) ->
          if toTreeModel model/=toTreeModel model' then return False
          else liftIO $ do
            row <- treeStoreGetTree model source
            treeStoreInsertTree model (init dest) (last dest) row
            return True
  }

