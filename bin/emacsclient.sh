#!/bin/bash
#
# Wrapper around emacsclient that also switches focus to the emacs window

hash emacsclient &>/dev/null # prefetch

if hash wmctrl &>/dev/null; then
    wmctrl -a emacs
    echo "Switched focus to Emacs window."
fi

exec emacsclient "$@"
