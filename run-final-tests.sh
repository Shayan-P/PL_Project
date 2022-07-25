rm tests/final/*.out
for t in $(ls tests/final/*.txt)
do
    racket main.rkt $t > $t.out
done
