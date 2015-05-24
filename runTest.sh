while true; do
        cabal build && runhaskell -Wall -isrc -itest test/Spec.hs
	date
	inotifywait -e modify -r  . --exclude '(.~|.swp|dist)'
done
