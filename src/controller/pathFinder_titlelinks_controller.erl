-module(pathFinder_titlelinks_controller, [Req]).
-compile(export_all).


list('GET', []) ->
  Titlelinks = boss_db:find(titlelink, []),
  {ok, [{titlelinks, Titlelinks}]}.

path('GET',[]) ->
  %No error checking occurs -- query_param will return undefined if not present
  %Should check that both parameters are present and occur in the database
  %Gets the from parameter
  From  = Req:query_param( "from" ),
  %Gets the to parameter
  To = Req:query_param( "to" ),
  %Spawns the collector with an empy paths list and 1 active process count
  CollectorPid = spawn(fun () -> collector(1,[],self()) end),
  %Starts the path finder "root" at the initial node
  path_finder(4,CollectorPid,From,To,[]),
  %This receive never occurs -- the remaining issue with the code
  receive
	{lists,Titlelinks} ->io:format("here ~p ~n",[Titlelinks])
  end,
  {ok, [{titlelinks, Titlelinks}]}.
 % {ok, [{titlelinks, collector(1,[],self())}]}.

%If the process count reaches 0 then the process is complete and it should pass all lists
%  back to the initial process -- currently prints all lists and the message passing doesn't work
collector(0, L,ReturnPid) -> io:format("Done ~nLists: ~p ~n",[L]),ReturnPid ! {lists,L};
collector(Count,L,ReturnPid) ->
	receive
		%message indicating that N more processes are spawned (should expect N more messages)
		{spawning,PID,N} ->
			%Responds to the process so that it can continue and spawn its children
			PID ! ok,
			 collector(Count + N - 1, L,ReturnPid);
		%Indicates that a path has been successfully found and that 1 processes is dying
		{list,NewList} -> 
			collector(Count - 1, [NewList | L],ReturnPid);
		%Returns the current count to the requesting process and restarts the loop
		{processes,PID} -> PID ! {running, Count}, collector(Count,L,ReturnPid)
	end.

%When the Current Word is the EndWord thn return a list to the collector
path_finder(_,Collector_PID,Word,EndWord,Path) 
	when Word =:= EndWord -> 
		Collector_PID ! {list,lists:append(Path,[EndWord])};
%When it has reached the maximum depth finish by messaging the collector
%  that the process is spawning 0 new processes
path_finder(0,Collector_PID,_,_,_) -> Collector_PID ! {spawning,self(),0};
%Reaching this state means that the depth is not reached and the 
%    word has not been found yet
path_finder(Remaining,Collector_PID,Word,EndWord,Path) ->
	%cannot call functions in guards so store the value here
	Contains = contains(Path,Word), 
	if
	%if the path contains the current word a cycle is occuring and exit
	Contains -> Collector_PID ! {spawning,self(),0};
	true->
	%gets a list of lists(strings) which are the next words  
	Words = next(Word),
	%signals the collector that a process will be spawned for each word
	Collector_PID ! {spawning,self(),length(Words)},
	%waits for the collector to have signaled back to eliminate race conditions
	receive
		ok -> ok
	end,
	%builds the path with the current word at the end
	NewPath = lists:append(Path, [Word]),
	lists:foreach(fun (W) -> 
		%spawns a process for each word
		spawn_pathFinder(Remaining - 1, Collector_PID, W, EndWord, NewPath)
		end,
		Words)
	end.

%spawns a collector only when the total number of pathFinders is less than
%  a set limit. Implemented as previous iterations crashed due to spawning
%  more processes than the system's process limit
spawn_pathFinder(N, Collector_PID, Word, EndWord, Path) ->
	Limit = 3000,
	Collector_PID ! {processes, self()},
	receive
		{running, Count} -> ok
	end,
	if
		Count >= Limit -> io:format("sleeping~n"),timer:sleep(100),spawn_pathFinder(N,Collector_PID,Word,EndWord,Path);
		true -> spawn(fun () -> path_finder(N,Collector_PID,Word,EndWord,Path) end)
	end.


%returns a list of lists(strings) of all words 
%  that are linked to the on the current word's page
next(Word) ->
   Words = boss_db:find(titlelink, [{page_title, 'equals', Word}]),
  %list comprehension to extract only the next values
  % and convert from binary to list values
  [binary_to_list(To) || {titlelink,_,From,To} <- Words].

%returns if a list contains an item or not
contains(List, Item) -> length(lists:filter( fun(X) -> X =:= Item end, List)) > 0.


%Type of is no longer used but returns the type of a variable
%   was used to determine the return type from boss_db:find
type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_bitstring(X) -> bitstring;  % will fail before e12
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;
type_of(_X)                     -> unknown.	
