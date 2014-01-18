-module(sat_worker).
-export([reciever/2]).

% --------------------
% validateTop(Ex) - Validates the expression
% --------------------
validateTop(Ex) -> try
					   case Ex of
						   [] -> false;
						   _ -> validate(Ex)
					   end
				   catch
					   _Exception:_Reason -> false
				   end.


% --------------------
% validate([]) - Helper function for validateTop()
% --------------------
validate([]) -> true;
validate([H|T]) -> case (isInts(H)) of
					   true -> validate(T);
					   false -> false
				   end.

% --------------------
% isInts() - Checks to see if the contents of the tuple is an int
% --------------------
isInts({X,Y,Z}) -> if
					   (not is_integer(X)) -> false;
					   (not is_integer(Y)) -> false;
					   (not is_integer(Z)) -> false;
					   true -> true
				   end.

% --------------------
% reciever(Ex,Parent) - Recieves the information from the parent process
% --------------------
reciever(Ex,Parent) -> top(Ex,Parent).

% --------------------
% top(Ex,Parent) - Checks to see if input is valid, then solves the 3-sat problem and sends the message back to the parent process
% --------------------
top(Ex,Parent) -> IsValid = validateTop(Ex),
		   case IsValid of
			   false -> Parent ! {worker,ignored};
			   true -> Parent ! {worker,trying},
				   	   Keys = fill_unused(find_keys(Ex)),
					   Result = check_perms(Ex,Keys,perms(length(Keys))),
					   Parent ! {worker,result, Result}
		   end.

% --------------------
% fill_unused(List) - Fills any unused keys
% --------------------
fill_unused(List) -> make_list(lists:last(List)).

% --------------------
% make_list(L) - Makes a list of Keys
% --------------------
make_list(L) -> [X || X <- lists:seq(1,L)].

% --------------------
% check_perms() - Checks all permutations against the 3-Sat expression
% --------------------
check_perms(_, _, []) -> unsat;
check_perms(Ex, Keys,[H|T]) -> TF = test_if_true(Ex,[Keys] ++ [H]),
							   if
								   TF -> H;
								   true -> check_perms(Ex,Keys,T)
							   end.

% --------------------
% test_if_true() - Tests if the Expression is true
% --------------------
test_if_true([],_) -> true;
test_if_true([H|T],Key_Value) -> evaluate_tuple(H,Key_Value) and test_if_true(T,Key_Value).

% --------------------
% evaluate_tuple() - Tests if the tuple is true
% --------------------
evaluate_tuple({X,Y,Z}, Key_Value) -> evaluate_value(X,Key_Value) or evaluate_value(Y,Key_Value) or evaluate_value(Z,Key_Value).

% --------------------
% evaluate_value() - Tests if the value is true
% --------------------
evaluate_value(N,Key_Value) -> if
								   N < 0 -> (not find_value(abs(N),Key_Value));
								   true -> find_value(N,Key_Value)
							   end.

% --------------------
% find_value() - Finds values for keys in the key, value lists
% --------------------
find_value(_,[[],[]]) -> valueNotFound;
find_value(N,[[K|T1],[V|T2]]) -> if
							 K =:= N -> V;
							 true -> find_value(N,[T1,T2])
					 end.

% --------------------
% find_keys() - Finds the keys in the expression
% --------------------
find_keys(Ex) -> lists:usort(find_keys_hlpr(Ex,[])).
%
% --------------------
% find_keys_hlpr() - Helper for find_keys()
% --------------------
find_keys_hlpr([],K) -> K;
find_keys_hlpr([H|T],K) -> find_keys_hlpr(T,find_keys_in_tuple(H,K)).
find_keys_in_tuple({X,Y,Z},K) -> [abs(X)] ++ [abs(Y)] ++ [abs(Z)] ++ K.
								 
% --------------------
% perms() - Finds the permutations of N given boolean values
% --------------------
perms(0) -> [[]];
perms(N) -> [[X|Y] || X<-[true,false], Y<-perms(N-1)].

% --------------------
% runTests() - Runs a test case
% --------------------
runTests() -> runCase("ListCase Size 7",listCase(7)),
			  ok.

% --------------------
% listCase() - Creates a test case
% --------------------
listCase(Limit) ->
	PosTuples = [ {X,X,X} || X <- lists:seq(1,Limit) ],
	NegTuples = [ {X,X,X} || X <- lists:seq(Limit*(-1),-1) ],
	lists:flatten(PosTuples ++ NegTuples).

% --------------------
% runCase() - Runs an individual case
% --------------------
runCase(S,C) -> io:fwrite("~s:\n",[S]),
				Timer = timer:tc(?MODULE,top,[C]),
				{Time,Result} = Timer,
				ResultStr = lists:flatten(io_lib:format("~p", [Result])),
				InputStr = lists:flatten(io_lib:format("~p", [C])),
				UnderOver = if
								(Time/1000000) =< 10 -> "";
								true -> " Over Time!"
							end,
				io:fwrite("\t~s -> ~s\n\t~w Seconds (~w Miliseconds)~s\n",[InputStr,ResultStr,(Time/1000000),(Time/1000),UnderOver]).

