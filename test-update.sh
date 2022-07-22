rm tests/*.out
for t in $(ls tests/*.mypy)
do
    racket main.rkt $t > $t.out
done
for t in $(ls tests/their/*.mypy)
do
    racket main.rkt $t > $t.out
done
