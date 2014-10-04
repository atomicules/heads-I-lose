-module(polyline).
-export([decode/1, eight_bit_chunks/1, five_bit_chunks/1, five_bit_chunk/1, bin_flip/1]).

%See https://developers.google.com/maps/documentation/utilities/polylinealgorithm


decode(Encoded_polyline) ->
	%The following are all bundled:
		%Convert Ascii character to numeric value
		%Subtract 63 from each value
		%"Un-or" the 0x20 bit
		%Un-reverse the 5 bit chunks
	Five_bit_chunks = five_bit_chunks(Encoded_polyline),
	%Get back to eight bit chunks
	Eight_bit_chunks = eight_bit_chunks(Five_bit_chunks).
	%Invert
	%To finish...


%Surely better way than this, but for now...
eight_bit_chunks(Five_bit_chunk_string) ->
	eight_bit_chunks_(Five_bit_chunk_string, []).
eight_bit_chunks_(Five_bit_chunk_string, List_of_eight_bit_chunks) when length(Five_bit_chunk_string) > 8 ->  
	%Is that a string or a list, might need to convert?
	Eight_bit_chunk = lists:sublist(Five_bit_chunk_string,1,8),
	Rest_of_five_bit_chunk_string = lists:nthtail(9,Five_bit_chunk_string),
	%io:format(Rest_of_five_bit_chunk_string++"~n"),
	eight_bit_chunks_(Rest_of_five_bit_chunk_string, [Eight_bit_chunk]++List_of_eight_bit_chunks);
eight_bit_chunks_(Five_bit_chunk_string, List_of_eight_bit_chunks) when length(Five_bit_chunk_string) =< 8, Five_bit_chunk_string /= [] ->  
	%io:format(Five_bit_chunk_string++"~n"),
	eight_bit_chunks_([], [Five_bit_chunk_string]++List_of_eight_bit_chunks);
eight_bit_chunks_([], List_of_eight_bit_chunks) ->
	lists:reverse(List_of_eight_bit_chunks).


five_bit_chunks(Encoded_polyline) ->
	five_bit_chunks_(Encoded_polyline,[]).
five_bit_chunks_([Head | Rest], List_of_chunks) ->
	Five_bit_chunk = five_bit_chunk(Head),
	%Add to Reversed_chunks
	five_bit_chunks_(Rest,[Five_bit_chunk]++List_of_chunks);
	%Something like that
five_bit_chunks_([], List_of_chunks) ->
	lists:reverse(List_of_chunks).


%What if Binary_chunk is shorter than 5?
five_bit_chunk(Ascii_bit) ->
	%subtract 63
	Shifted_bit = Ascii_bit - 63,
	%Convert to binary
	%From http://erlangcentral.org/wiki/index.php/Converting_Between_Binary_and_Decimal
	Binary_chunk = hd(io_lib:format("~.2B", [Shifted_bit])),
	%"Un-or" the value, get 5 bit chunk
	%not the fanciest way, but...
	lists:sublist(Binary_chunk,2,6).


%bnot doesn't seem to work as I thought it would so do it very inelegantly by switching each "bit" in a string.
%This is pretty close just need to decide whether want to return string or number.
bin_flip(Binary_number) ->
	Binary_string =	hd(io_lib:format("~.2B", [Binary_number])),
	bin_flip_(Binary_string, []).
bin_flip_([Head | Rest], Flipped_string) ->
	Head_bit = hd(io_lib:format("~c",[Head])),
	Flipped_bit = if Head_bit =:= "0" ->
		"1";
	true ->
		"0"
	end,
	bin_flip_(Rest, Flipped_bit++Flipped_string);
bin_flip_([], Flipped_string) ->
	lists:reverse(Flipped_string).


%Notes
	%To enter numbers of a different base 2#101010
	%0x1f is 31, 0x20 is 32
	%Need to ensure functions are tail recursive
	%To convert to integer, just need to do $<char>
	%just do hd(String) will come as a number
	%Last = hd(io_lib:format("~c", [hd(lists:reverse(hd(io_lib:format("~.2B", [Binary_number]))))])),
	%{ok, Almost_flipped, _} = io_lib:fread("~2u",Almost_flipped_string),
