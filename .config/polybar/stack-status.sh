#!/usr/bin/env sh

stack=$(pgrep -a bwrap | grep stack | grep AppImage | wc -l)

if [[ "$stack" == "2" ]]; then
    echo "";
    exit 0;
fi

echo "󰓨";
