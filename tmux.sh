#!/bin/bash
NAME="gui_scad-dev";

cd ~/gui_scad

tmux new-session -s $NAME -d
tmux split-window
tmux split-window
tmux select-layout -t $NAME main-vertical
tmux send-keys "sh runTest.sh" C-m
tmux select-pane -t 0
tmux attach-session -t $NAME
