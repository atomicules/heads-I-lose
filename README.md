#Heads I Lose

A little learning exercise in Erlang. It looks up the wind direction for my cycle commute and lets me know whether I win (tail wind) or lose (head wind).

##Usage

1. Compile with `erlc headsilose.erl`.
2. Run with `erl -run headsilose start <location id> <heading> -noshell -s init stop`.

##Todo

- Return location IDs properly rather than the XML dump
