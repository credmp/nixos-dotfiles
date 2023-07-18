#!/bin/sh

count=$(pgrep -a openvpn$ | head -n 1 |wc -l)

if [[ "$count" == "1" ]]; then
  echo "";
else
  echo "";
fi

#printf "" && (pgrep -a openvpn$ | head -n 1 | awk '{print $NF }' | cut -d '.' -f 1 && echo "") | echo ""
