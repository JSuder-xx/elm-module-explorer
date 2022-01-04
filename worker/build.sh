elm make src/Main.elm --optimize --output=../out/elm-type-explorer.js
uglifyjs ../out/elm-type-explorer.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output ../out/elm-type-explorer.min.js
rm ../out/elm-type-explorer.js
