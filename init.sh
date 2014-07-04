cabal sandbox delete
cabal sandbox init
cabal sandbox add-source servant/
cabal sandbox add-source servant-pool/
cabal sandbox add-source servant-postgresql/
cabal sandbox add-source servant-scotty/
cabal install servant servant-pool servant-postgresql servant-scotty --only-d