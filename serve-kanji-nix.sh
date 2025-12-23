spago bundle --module Demo.OnlyKanji --outfile ./web/kanji.js --platform browser --bundle-type app
node ./node_modules/parcel/lib/bin.js build ./web/kanji.html --no-cache
node ./node_modules/parcel/lib/bin.js serve ./web/kanji.html --no-cache --port 1340
