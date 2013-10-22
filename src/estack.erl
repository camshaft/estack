-module(estack).

-export([execute/3]).
-export([resume/6]).

execute(Req, Env, []) ->
  {ok, Req, Env};
execute(Req, Env, [Fun|Stack]) ->
  call(Req, Env, normailze(Fun), Stack).

resume(Req, Env, Stack, Module, Function, Args) ->
  call(Req, Env, fun(_, _) ->
    apply(Module, Function, Args)
  end, Stack).

call(Req, Env, Fun, Stack) ->
  case catch Fun(Req, Env) of
    {ok, Req2, Env2} ->
      execute(Req2, Env2, Stack);
    {suspend, Module, Function, Args} ->
      erlang:hiberate(?MODULE, resume,
          [Req, Env, Stack, Module, Function, Args]);
    {halt, Req2} ->
      {halt, Req2};
    {error, StatusCode, Req2} ->
      {error, StatusCode, Req2};
    %% TODO is there a way to check funtion arity?
    {'EXIT', {{badarity, _}, _}} ->
      call(Req, Env, fun(_, _) ->
        Fun(Req, Env, Stack)
      end, []);
    {'EXIT', {{badmatch, {halt, Req2}}, _}} ->
      {halt, Req2};
    {'EXIT', {{badmatch, {error, StatusCode, Req2}}, _}} ->
      {error, StatusCode, Req2}
  end.

normailze(Fun) when is_atom(Fun) ->
  fun Fun:execute/2;
normailze(Fun) when is_function(Fun) ->
  Fun;
normailze({Module, Fun}) ->
  fun Module:Fun/2.
