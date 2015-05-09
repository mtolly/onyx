#!/usr/bin/env fish
rm -f parsed.txt
for f in ~/git/hmx-charts/rb3/*/*.mid
  echo "" >> parsed.txt
  echo $f >> parsed.txt
  echo "" >> parsed.txt
  dist/build/testparser/testparser $f ^^ parsed.txt
end
