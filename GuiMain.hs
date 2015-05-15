import Graphics.UI.Gtk
import Data.IORef

import Gui
import ScadTreeView

main :: IO()
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

    treeStore <- createEmptyTreeStore
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

