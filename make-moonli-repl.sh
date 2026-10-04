#!/bin/bash

sbcl --noinform --no-userinit \
     --eval '(require :asdf)' \
     --eval '(asdf:load-system "moonli/repl")' \
     --eval '(in-package :moonli-user)' \
     --eval '(asdf:make "moonli/repl")' \
     --eval '(uiop:quit)'
