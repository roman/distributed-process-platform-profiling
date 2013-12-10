#!/bin/bash

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT=$(cd $THIS_DIR/.. && pwd)

clone_dependencies () {
    [[ -d $ROOT/vendor ]] || mkdir $ROOT/vendor
    cat $ROOT/DEPENDENCIES | while read url; do
        REPO_NAME=$(echo $url | sed -e "s;https://github.com/haskell-distributed/;;")
        [[ -d "$ROOT/vendor/$REPO_NAME" ]] || git clone -b development $url $ROOT/vendor/$REPO_NAME
    done
}

clone_dependencies
