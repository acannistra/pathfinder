-module(pathFinder_titlelinks_controller, [Req]).
-compile(export_all).

list('GET', []) ->
  Titlelinks = boss_db:find(titlelink, []),
  {ok, [{titlelinks, Titlelinks}]}.

path('GET',[]) ->
  From  = Req:query_param( "from" ),
  To = Req:query_param( "to" ),
  CollectorPid = spawn(fun () -> collector(1,[],self()) end),
  path_finder(10,CollectorPid,From,To,[]),
  receive
	{lists,Titlelinks} ->io:format("here ~p ~n",[Titlelinks])
  end,
  {ok, [{titlelinks, Titlelinks}]}.
 % {ok, [{titlelinks, collector(1,[],self())}]}.


collector(0, L,ReturnPid) -> io:format("Done ~nLists: ~p ~n",[L]),ReturnPid ! {lists,L};
collector(Count,L,ReturnPid) ->
	receive
		{spawning,PID,N} ->
			PID ! ok,
			 collector(Count + N - 1, L,ReturnPid);
		{list,NewList} -> 
			collector(Count - 1, [NewList | L],ReturnPid)
	end.

path_finder(_,Collector_PID,Word,EndWord,Path) 
	when Word =:= EndWord -> 
		Collector_PID ! {list,lists:append(Path,[EndWord])};
path_finder(0,Collector_PID,_,_,_) -> Collector_PID ! {spawning,self(),0};
path_finder(Remaining,Collector_PID,Word,EndWord,Path) ->
	Contains = contains(Path,Word),
	if
	Contains -> Collector_PID ! {spawning,self(),0};
	true->  
	Words = next(Word),
	Collector_PID ! {spawning,self(),length(Words)},
	receive
		ok -> ok
	end,
	NewPath = lists:append(Path, [Word]),
	lists:foreach(fun (W) -> 
		spawn(fun () -> 
		path_finder(Remaining - 1, Collector_PID,W,EndWord,NewPath)
		end)
		end,
		Words)
	end.

next(Word) ->
   Words = boss_db:find(titlelink, [{page_title, 'equals', Word}]),
  [binary_to_list(To) || {titlelink,_,From,To} <- Words].

contains(List, Item) -> length(lists:filter( fun(X) -> X =:= Item end, List)) > 0.

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
