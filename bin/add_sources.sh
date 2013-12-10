#!/bin/bash

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT=$(cd $THIS_DIR/.. && pwd)

add_sources () {
    cd $ROOT
    cabal sandbox init
    cat $ROOT/DEPENDENCIES | while read url; do
        PROJECT_NAME=$(echo $url | sed -e "s;https://github.com/haskell-distributed/;;")
        DEPENDENCY=$ROOT/vendor/$PROJECT_NAME
        [[ -d $DEPENDENCY ]] && cabal sandbox add-source $DEPENDENCY
    done
    cd -
}

add_sources
