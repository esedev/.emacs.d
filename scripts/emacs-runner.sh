#!/bin/bash

if ! command -v emacs 2>&1 >/dev/null; then
    echo "Command 'emacs' not founded"
    exit 1
fi

for a in "$@"; do
    if [[ "$a" == "-nw" || "$a" == "--no-window-system" || "$a" == "-t" || "$a" == "--tty" ]]; then
        is_terminal=true
    fi
    if [[ "$a" == "-s" || "$a" == "--socket-name"* ]]; then
        is_client=true
    fi
    if [[ "$a" == "--daemon"* || "$a" == "--bg-daemon"* || "$a" == "--fg-daemon"* ]]; then
        is_daemon=true
        echo "Start EMACS as $a"
    fi
done
unset a

if [ "$is_client" != true ]; then
    COMMAND=emacs
else
    COMMAND=emacsclient
fi

export LSP_USE_PLISTS=true

if [ "$is_terminal" == true ] || [ "$is_daemon" == true ]; then
    $COMMAND "$@"
else
    nohup $COMMAND "$@" &>/dev/null & disown %%
fi
