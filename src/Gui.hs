module Gui
(
    Gui(..),
    getTreeModel
)
where

import Data.IORef
import Graphics.UI.Gtk

import ScadDSL

data Gui = Gui
    {
        _builder :: Builder,
        _treeStore :: TreeStore Scad,
        _treeView :: TreeView,
        _menu :: IORef (Maybe Menu) -- Hack to make sure that the GC(?) will not remove the menu when used
    }

getTreeModel :: Gui -> IO(TreeModel)
getTreeModel gui = do
    let treeView = _treeView gui
    maybeModel <- treeViewGetModel treeView
    case maybeModel of
        Just model -> return model
        Nothing -> error "TreeView did not have a TreeModel set"
