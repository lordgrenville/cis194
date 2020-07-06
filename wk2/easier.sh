awk '{ if ($1 == "E" && $2 >= 50) {print}}' error.log | sort -k 3n | cut -d' ' -f4-
echo "Solution to bonus riddle is:"
awk '{ if ($1 == "E" && $2 >= 50) {print}}' error.log | sort -k 3n | cut -d' ' -f4- | cut -c 1
