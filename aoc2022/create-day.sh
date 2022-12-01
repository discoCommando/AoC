if [ $# -eq 0 ]
  then
    echo "Usage: ./create-day.sh 03" 
    exit 1
fi
touch ./data/$1.txt
cp ./test/Day00Spec.hs ./test/Day$1Spec.hs
cp ./src/Day00.hs ./src/Day$1.hs
gen-hie > hie.yaml
