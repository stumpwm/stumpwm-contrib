#!/bin/sh
ARGUMENTS="sbcl --noinform"
cd $(dirname $(realpath $0))
if [[ -f ~/.sbclrc ]] 
then
  ARGUMENTS="${ARGUMENTS} --load $(realpath ~/.sbclrc)"
fi
shift
exec ${ARGUMENTS} --script test.lisp $@
