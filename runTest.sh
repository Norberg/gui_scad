while true; do
	runhaskell -isrc -itest test/Spec.hs
	date
	inotifywait -e modify -r  . --exclude '(.~|.swp|dist)'
done
