#Heads I Lose

A little learning exercise in Erlang. It looks up the wind direction ([via the MetOffice Datapoint API](http://www.metoffice.gov.uk/datapoint/api)) for my cycle commute and lets me know whether I win (tail wind) or lose (head wind).

##Usage

1. Get an [API key](http://www.metoffice.gov.uk/datapoint/support/API) and save in a file called `~/.datapoint`.
2. Compile with `erlc headsilose.erl`.
3. Run `erl -run headsilose get_locations <Optional search term> -noshell -s init stop` to get a list of locations and Ids. The optional search term cannot contain spaces.
4. Then run `erl -run headsilose headsilose <location id> <heading in 16-point compass format> -noshell -s init stop` to get the result.

Since I wrote this to be semi-useful for me, the result returned depends on the time of the day. If it's run before 8am it looks for the 6am weather data (since data is in 3 hour periods), run between 8am and 7pm it looks for the 6pm data for going home and run after that time it looks again for the 6am data, but for the next day.

Heading is the overall direction of travel in 16-point compass format. So a heading of N means going from south to north. I only mention this because wind directions are "directions from".


##Credits

Various posts I've found that have helped me out:

- Initial inspiration from [PragDave - A First Erlang Program](http://pragdave.pragprog.com/pragdave/2007/04/a_first_erlang_.html)
- Putting initial inspiration to practice (i.e. `inets:start`) from [Andrew Locatelli Woodcock - Connecting to Cloudant from Erlang: a quick example of using HTTPS from httpc:request](http://andrewlocatelliwoodcock.com/2012/06/12/connecting-to-cloudant-from-erlang-a-quick-example-of-using-https-from-httpcrequest-17-2/)
- The recursive parsing of XML from [Sam Ruby - Parsing Atom with Erlang](http://intertwingly.net/blog/2007/08/28/Parsing-Atom-with-Erlang)
- Formating dates with leading zeroes from [Warren Young on Stack Overflow](http://stackoverflow.com/a/7599506/208793)
- Figuring out command line arguments from [Cody on Stack Overflow](http://stackoverflow.com/a/8498073/208793)

##Todo

- Make it factor in wind speed and gusts to the win/lose decision as opposed to just direction.
- Report out a bit more info in addition to the win/lose/draw.
- Sort the list of locations?
- Error handling, because the API does seem to be a bit flaky.
