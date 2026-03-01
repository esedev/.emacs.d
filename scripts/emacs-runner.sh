#!/bin/bash

if ! command -v emacs 2>&1 >/dev/null; then
    echo "Command 'emacs' not founded"
    exit 1
fi

COMMAND=emacs

for a in "$@"; do
    if [[ "$a" == "-s" || "$a" == "--socket-name"* ]]; then
        COMMAND=emacsclient
    fi
    if [[ "$a" == "-nw" || "$a" == "--no-window-system" ]]; then
        is_terminal=true
    fi
    if [[ "$a" == "--daemon"* || "$a" == "--bg-daemon"* || "$a" == "--fg-daemon"* ]]; then
        is_daemon=true
        echo "Start EMACS as '$a'"
    fi
done
unset a

if [ "$is_terminal" == true ] || [ "$is_daemon" == true ]; then
    $COMMAND "$@"
else
    nohup $COMMAND "$@" &>/dev/null & disown %%
fi
