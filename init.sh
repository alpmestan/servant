cabal sandbox delete
cabal sandbox init
cabal sandbox add-source servant/
cabal sandbox add-source servant-example/
cabal sandbox add-source servant-response/
cabal sandbox add-source servant-pool/
cabal sandbox add-source servant-postgresql/
cabal sandbox add-source servant-scotty/
cabal install servant servant-pool servant-response servant-postgresql servant-scotty servant-example --only-d