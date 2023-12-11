if [ $# -eq 0 ]
  then
    echo "Usage: ./create-day.sh 03" 
    exit 1
fi
touch ./data/$1.txt
cp ./test/Day00Spec.hs ./test/Day$1Spec.hs
sed -i '' -e s/00/${1}/g ./test/Day$1Spec.hs
cp ./src/Day00.hs ./src/Day$1.hs
sed -i '' -e s/00/${1}/g ./src/Day$1.hs
sed -i '' -e s/Day[0-9]*/Day${1}/g ./app/Main.hs
hpack
gen-hie > hie.yaml
