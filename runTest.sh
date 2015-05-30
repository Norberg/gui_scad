while true; do
        cabal build && runhaskell -Wall -isrc -itest test/Spec.hs -f progress
	date
	inotifywait -e modify -r  . --exclude '(.~|.swp|dist)'
done
