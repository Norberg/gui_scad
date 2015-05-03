import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

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

    widgetShowAll winMain
    mainGUI


save builder = do
    putStrLn "Saving..."
    txtValue <- builderGetObject builder castToEntry "txtValue"
    valueString <- entryGetText txtValue
    putStrLn valueString

    txtName <- builderGetObject builder castToEntry "txtName"
    valueName <- entryGetText txtName
    putStrLn valueName
