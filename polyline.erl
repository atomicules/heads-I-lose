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
	Eight_bit_chunks = eight_bit_chunks(Five_bit_chunks),
	%Invert if negative, if last bit is a 1
	%To finish...
	Last_bit = [hd(lists:reverse(hd(lists:reverse(Eight_bit_chunks))))],
	Flipped_chunks = lists:map(fun(Chunk) ->
		bin_flip_(Chunk, []) end,
		Eight_bit_chunks),
	Chunks = if Last_bit =:= "1" ->
		Flipped_chunks;
	true ->
		Eight_bit_chunks
	end,
	%Now want as one long binary and right shift one bit
	{ok, [Flattened_binary], []} = io_lib:fread("~2u", lists:flatten(Chunks)),
	Shifted_binary = Flattened_binary bsr 1,
	%If negative need to take away one and invert again
	Final_binary = if Last_bit =:= "1" ->
		bin_flip(Shifted_binary - 1);
	true ->
		Shifted_binary
	end,
	Final_binary.
	

%Surely better way than this, but for now...
eight_bit_chunks(List_of_five_bit_chunks) ->
	Five_bit_chunk_string = lists:reverse(lists:flatten(List_of_five_bit_chunks)),
	eight_bit_chunks_(Five_bit_chunk_string, []).
eight_bit_chunks_(Five_bit_chunk_string, List_of_eight_bit_chunks) when length(Five_bit_chunk_string) > 8 ->  
	Eight_bit_chunk = lists:reverse(lists:sublist(Five_bit_chunk_string,1,8)),
	Rest_of_five_bit_chunk_string = lists:nthtail(8,Five_bit_chunk_string),
	%io:format(Rest_of_five_bit_chunk_string++"~n"),
	eight_bit_chunks_(Rest_of_five_bit_chunk_string, [Eight_bit_chunk]++List_of_eight_bit_chunks);
eight_bit_chunks_(Five_bit_chunk_string, List_of_eight_bit_chunks) when length(Five_bit_chunk_string) =< 8, Five_bit_chunk_string /= [] ->  
	%io:format(Five_bit_chunk_string++"~n"),
	Padded_bit_string = pad_to(8, lists:reverse(Five_bit_chunk_string)),
	eight_bit_chunks_([], [Padded_bit_string]++List_of_eight_bit_chunks);
eight_bit_chunks_([], List_of_eight_bit_chunks) ->
	%No need to reverse. Already done
	%lists:reverse(List_of_eight_bit_chunks).
	List_of_eight_bit_chunks.


five_bit_chunks(Encoded_polyline) ->
	five_bit_chunks_(Encoded_polyline,[]).
five_bit_chunks_([Head | Rest], List_of_chunks) ->
	Five_bit_chunk = five_bit_chunk(Head),
	%Add to Reversed_chunks
	five_bit_chunks_(Rest,[Five_bit_chunk]++List_of_chunks);
	%Something like that
five_bit_chunks_([], List_of_chunks) ->
	%Since want it reversed just leave as is
	List_of_chunks.


five_bit_chunk(Ascii_bit) ->
	%subtract 63
	Shifted_bit = Ascii_bit - 63,
	%Convert to binary
	%From http://erlangcentral.org/wiki/index.php/Converting_Between_Binary_and_Decimal
	Binary_chunk = hd(io_lib:format("~.2B", [Shifted_bit])),
	%What if Binary_chunk is shorter than 6?
	Padded_chunk = pad_to(6, Binary_chunk),
	%"Un-or" the value, get 5 bit chunk
	%not the fanciest way, but...
	lists:sublist(Padded_chunk,2,6).


%I can't figure out padding with io:format etc when printing binary numbers
pad_to(Length, Binary_string) when length(Binary_string) < Length ->
	Padded_binary_string = "0"++Binary_string,
	pad_to(Length, Padded_binary_string);
pad_to(Length, Binary_string) when length(Binary_string) == Length ->
	Binary_string.
	

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
