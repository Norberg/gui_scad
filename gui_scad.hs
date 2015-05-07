import Data.Tree
import Data.Maybe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Control.Monad.IO.Class (liftIO)
import Control.Monad

import ScadDSL

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

    createTreeView builder

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

--createTreeView :: Builder -> IO (ConnectId TreeSelection)
createTreeView builder = do
    treeView <- builderGetObject builder castToTreeView "treeView"

    --let tree = Node "A" [Node "B" [], Node "C" [Node "D" [], Node "E" []]]
    let tree = Node  Union [Node (Sphere 1.0) [], Node (Cube 2.0) []]
    let forest = [tree, tree]

    treeStore <- treeStoreNew forest
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
                                               theTime <- eventTime
                                               case button of
                                                    RightButton -> liftIO (mouseButtonPressed theTime)
                                            ))

mouseButtonPressed :: TimeStamp -> IO ()
mouseButtonPressed theTime = do
    menu <- menuNew
    menuItem <- menuItemNewWithLabel "Add 3D object"
    on menuItem menuItemActivated (putStrLn "3D object added")
    menuShellAppend menu menuItem
    menuItem <- menuItemNewWithLabel "Add Transformations"
    on menuItem menuItemActivated (putStrLn "Transformation added")
    menuShellAppend menu menuItem
    widgetShowAll menu
    menuPopup menu (Just (RightButton, theTime))

nodeSelected treeStore treeSelection = do
    maybeTreeIter <- treeSelectionGetSelected treeSelection
    let treeIter = fromJust maybeTreeIter
    treePath <- treeModelGetPath treeStore treeIter
    value <- treeStoreGetValue treeStore treePath
    putStrLn $ "Selected node:" ++ show (value)
    

oneSelection :: ListStore String -> TreeSelection ->  IO ()
oneSelection list tree = do
   sel <- treeSelectionGetSelectedRows tree
   let s = head  (head sel)
   v <- listStoreGetValue list s
   putStrLn $ "selected " ++ v
