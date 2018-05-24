#!/usr/bin/env bash

# Get datasets (that are too big for git)

function getGz() {
  mkdir executables/input
  local DST="./executables/input/${1}/"
  local FILE="${2}.txt"
  local FILEDST="${DST}/${FILE}"

  mkdir -p "${DST}"
  if [ ! -e "${FILEDST}" ]; then
    wget --directory-prefix="${DST}" "https://snap.stanford.edu/data/${4:-}/${FILE}.gz" \
      && gunzip "${DST}/${FILE}.gz" \
      && if [ -n "${3}" ]
      then
          sed -i '1,4d' "${FILEDST}"
      fi
  else
    echo skipping "${FILEDST}"
  fi
}

getGz "web" "web-Google" "true"
getGz "p2p" "p2p-Gnutella31" "true"
getGz "web" "wiki-Talk" "true"
getGz "web" "as-skitter" "true"
getGz "web" "soc-sign-Slashdot090221" "true"
getGz "socialNetwork/gowalla" "loc-gowalla_edges" ""
getGz "web" "cit-Patents" "true"
getGz "socialNetwork/pokec" "soc-pokec-relationships" ""
getGz "web" "com-dblp.ungraph" "true" "bigdata/communities/"

# getGz "web/so/" "sx-stackoverflow" ""
# FILE="input/web/so/sx-stackoverflow.txt"
# cut -f1,2 -d" " "${FILE}" > "${FILE}.1" && mv "${FILE}.1" "${FILE}"
