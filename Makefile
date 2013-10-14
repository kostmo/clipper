
#
# Depends on clipper lib
# /usr/local/include/polyclipping/clipper.hpp
# http://www.angusj.com/delphi/clipper.php
#

# This is needed because clipper lib is C++,
# C compiler barfs at it
BUILD_OPTS:=--with-gcc=g++ --hsc2hs-options='-k -C -fpermissive'

build:
	cabal -v build $(BUILD_OPTS)

install:
	cabal install $(BUILD_OPTS)

repl:
	cabal -v repl $(BUILD_OPTS)

nuke:
	git clean -fdx .

clean:
	rm -rf dist
