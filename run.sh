# Run code in a certain day.
# example: ./run.sh 2019 01

if [ $# -ne 2 ]; then
    echo "ERROR: Please provide year and day (example: $0 2019 01)"
    exit -1
fi

YEAR=$1
DAY=$2

if ! { [ -f "src/Y$YEAR/Day$DAY.hs" ] && [ -f "inputs/Y$YEAR/Day$DAY.txt" ]; }; then
    echo "ERROR: Y$YEAR/Day$DAY file does not exist."
    exit -1
fi

echo "Running Y$YEAR/Day$DAY..."
echo ""

cp src/Main.hs src/Main.hs.tmp
sed -i "s/import Y....\.Day../import Y$YEAR.Day$DAY/" src/Main.hs
stack run < inputs/Y$YEAR/Day$DAY.txt
cp src/Main.hs.tmp src/Main.hs
rm src/Main.hs.tmp
