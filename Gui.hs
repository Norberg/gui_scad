module Gui
(
    Gui(..),
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

