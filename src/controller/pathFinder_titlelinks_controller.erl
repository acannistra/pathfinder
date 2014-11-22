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


next(Word) ->
   Words = boss_db:find(titlelink, [{page_title, 'equals', Word}]),
   [To || {titlelink,_,From,To} <- Words].
	
