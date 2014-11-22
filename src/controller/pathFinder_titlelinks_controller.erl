-module(pathFinder_titlelinks_controller, [Req]).
-compile(export_all).

list('GET', []) ->
  Titlelinks = boss_db:find(titlelink, []),
  {ok, [{titlelinks, Titlelinks}]}.

path('GET',[]) ->
  From  = Req:query_param( "from" ),
  To = Req:query_param( "to" ),
  Titlelinks = next(From),
  {ok, [{titlelinks, Titlelinks}]}.


collector(0, L) -> L;
collector(Count,L) ->
	receive
		{spawning,N} -> collector(Count + N - 1, L);
		{list,NewList} -> collector(Count - 1, [NewList | L])
	end.
	

next(Word) ->
   Words = boss_db:find(titlelink, [{page_title, 'equals', Word}]),
   [To || {titlelink,_,From,To} <- Words].
	
