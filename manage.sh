#/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $DIR/../../../manageUtils.sh

mirroredProject eventmatch

BASE=$HGROOT/programs/data/eventmatch

case "$1" in
mirror)
  syncHg  
;;

esac

