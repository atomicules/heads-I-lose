#Heads I Lose

This started as a little learning exercise in Erlang, but I've been building on it to make it more useful for my cycle commute. It looks up the wind direction ([via the MetOffice Datapoint API](http://www.metoffice.gov.uk/datapoint/api)) and for a given route (a polyline from [OSRM](http://project-osrm.org/)) determines how much of the route is a headwind sidewind or tailwind. 

##Usage

Since it has just been developed for personal use installation is not very polished.

1. Get a MetOffice Datapoint [API key](http://www.metoffice.gov.uk/datapoint/support/API) and save in a file called `~/.datapoint`.
2. Get/install [Jiffy](https://github.com/davisp/jiffy). (I just cloned the repository, issued `make` and copied the `ebin` and `priv` directories to my `heads-I-lose` directory).
3. Within the `heads-I-lose` directory, compile with `erlc headsilose.erl; erlc weather_types.erl; erlc osrm.erl; erlc polyline.erl`.
4. Run `erl -run headsilose get_locations <Optional search term> -noshell -s init stop` to get a list of locations and Ids. The optional search term cannot contain spaces.
5. From within the erlang shell (because I don't yet know how to pass negative numbers on the command line) run `osrm:get_route([<start_lat> ,<start_lon>],[<end_lat>,<end_lon>])` to get a route from OSRM. You'll have to find the latitude and longitudes by [some other means](http://www.uk-postcodes.com/)) for now. This will save the route in `~/.headsilose-route`.
6. Run `erl -run headsilose headsilose <location id> -noshell` to get the result.

Since I wrote this to be semi-useful for me, the result returned depends on the time of the day. If it's run before 8am it looks for the 6am weather data (since data is in 3 hour periods) and assumes the route is being traversed normally, run between 8am and 7pm it looks for the 6pm data for going home and therefore also traverses the saved route in reverse, and run after that time it looks again for the 6am data, but for the next day, and thus the route is back to being traversed in the normal direction.

_Hint:_ I have a shell function defined as follows:

	function headsilose {
		erl -pa /home/simon/Code/github/atomicules/heads-I-lose /home/simon/Code/github/atomicules/heads-I-lose/ebin -run headsilose headsilose XXXXXX -noshell;
	}

So I can just call

	headsilose

Which will result in something like the following being printed out:

	It's a draw
	47.1% Headwind
	51.45% Sidewind
	1.45% Tailwind
	Direction: SSW
	Speed: 13 mph
	Gust: 29 mph
	Weather type: Cloudy
	Temperature: 4 deg C

_Note:_ I don't use [init stop](http://erlangcentral.org/wiki/index.php?title=Running_Erlang_Code_From_The_Command_Line&oldid=2293) in my main command line call as I have that in my script instead. Otherwise, if `headsilose` errors out then `init stop` will crash out (I guess because it is trying to stop something that isn't running).

##Credits

Various posts I've found that have helped me out:

- Initial inspiration from [PragDave - A First Erlang Program](http://pragdave.pragprog.com/pragdave/2007/04/a_first_erlang_.html)
- Putting initial inspiration to practice (i.e. `inets:start`) from [Andrew Locatelli Woodcock - Connecting to Cloudant from Erlang: a quick example of using HTTPS from httpc:request](http://andrewlocatelliwoodcock.com/2012/06/12/connecting-to-cloudant-from-erlang-a-quick-example-of-using-https-from-httpcrequest-17-2/)
- The recursive parsing of XML from [Sam Ruby - Parsing Atom with Erlang](http://intertwingly.net/blog/2007/08/28/Parsing-Atom-with-Erlang)
- Formating dates with leading zeroes from [Warren Young on Stack Overflow](http://stackoverflow.com/a/7599506/208793)
- Figuring out command line arguments from [Cody on Stack Overflow](http://stackoverflow.com/a/8498073/208793)
- Figuring out [Jiffy](http://www.snip2code.com/Snippet/51463/how-to-support-chinese-in-http-request-b/)
- [Rounding numbers](http://www.codecodex.com/wiki/index.php?title=Round_a_number_to_a_specific_decimal_place#Erlang)
- [Converting between binary and decimal](http://erlangcentral.org/wiki/index.php/Converting_Between_Binary_and_Decimal)

I did read through a [number](http://www.mathworks.com/matlabcentral/fileexchange/32341-google-maps-api-polyline-decoder) [of](http://jeffreysambells.com/2010/05/27/decoding-polylines-from-google-maps-direction-api-with-java) [posts](http://seewah.blogspot.co.uk/2009/11/gpolyline-decoding-in-python.html)/[implementations](http://unitstep.net/blog/2008/08/02/decoding-google-maps-encoded-polylines-using-php/) [of](https://github.com/Project-OSRM/osrm-frontend/blob/master/WebContent/routing/OSRM.RoutingGeometry.js) polyline decoders, but they didn't really help me in Erlang so I just worked backwards through the [specification](https://developers.google.com/maps/documentation/utilities/polylinealgorithm) and wrote my own, much less concise, version rather than porting an existing implementation.

##Todo

_I should really put these as issues_

- Sort the list of locations?
- Finish off implementing the list of client "supposed to"s from the OSRM API, such as checksum and hint data, but this would mean caching all previous requests made.
- Use postcodes to get the latitudes and longitudes via [UK Postcodes](http://www.uk-postcodes.com/)
- Figure out how to pass negative numbers on the command line (perhaps use [getopt](http://github.com/jcomellas/getopt)?)
- Use tuples instead of lists where it makes sense
