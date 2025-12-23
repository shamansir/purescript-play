spago bundle --module Demo.OnlyKanji --outfile ./web/kanji.js --platform browser --bundle-type app
parcel build ./web/kanji.html --no-cache
parcel serve ./web/kanji.html --no-cache
