if ! [ -x "$(command -v cmtk)" ]; then
   curl -OL https://raw.githubusercontent.com/jefferis/nat/master/build-cmtk.sh
   bash build-cmtk.sh
else 
   echo 'cmtk already installed'
fi