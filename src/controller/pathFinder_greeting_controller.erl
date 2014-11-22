-module(pathFinder_greeting_controller, [Req]).
-compile(export_all).

hello('GET', []) ->
 {ok, [{greeting, "Hello"}]}.

goodbye('GET', []) ->
 {ok, [{test, "Test worked!"}]}.
