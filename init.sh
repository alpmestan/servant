for pkg in `ls | grep servant | sort`;
do
  echo "Initializing $pkg..."
  cd $pkg/
  cabal sandbox init && cabal sandbox add-source ../servant/
  cabal install --only-d
  cd ../
  echo "Done with $pkg"
done