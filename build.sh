for pkg in `ls | grep servant | sort`;
do
  echo "Building $pkg..."
  cd $pkg/
  cabal configure -v0 && cabal build -v0
  cd ../
  echo "Done with $pkg"
done