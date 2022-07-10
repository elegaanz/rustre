#!/usr/bin/fish

cd ../lustre-v6

for f in (find -type f -name '*.lus')
  iconv -f ISO-8859-1 -t UTF-8 $f > $f.utf
  mv $f.utf $f
  echo done with $f
end
