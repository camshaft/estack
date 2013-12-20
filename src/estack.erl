-module(estack).

-export([execute/3]).
-export([resume/6]).

-define (STOP(Req, Env, Stack),
    {suspend, Module, Function, Args} ->
      erlang:hiberate(?MODULE, resume,
          [Req, Env, Stack, Module, Function, Args]);
    {halt, Req2} ->
      {halt, Req2};
    {error, StatusCode, Req2} ->
      {error, StatusCode, Req2}).

execute(Req, Env, []) ->
  {ok, Req, Env};
execute(Req, Env, [Fun|Stack]) ->
  call(Req, Env, normailze(Fun), Stack).

resume(Req, Env, Stack, Module, Function, Args) ->
  call(Req, Env, fun(_, _) ->
    apply(Module, Function, Args)
  end, Stack).

call(Req, Env, {Fun, 3}, Stack) ->
  Next = fun(Req2, Env2) ->
    execute(Req2, Env2, Stack)
  end,
  case catch Fun(Req, Env, Next) of
    {ok, _, _} = Res ->
      Res;
    {'EXIT', {{badmatch, {halt, Req2}}, _}} ->
      {halt, Req2};
    {'EXIT', {{badmatch, {error, StatusCode, Req2}}, _}} ->
      {error, StatusCode, Req2};
    ?STOP(Req, Env, [])
  end;
call(Req, Env, {Fun, 2}, Stack) ->
  case Fun(Req, Env) of
    {ok, Req2, Env2} ->
      execute(Req2, Env2, Stack);
    ?STOP(Req, Env, Stack)
  end.

normailze(Fun) when is_atom(Fun) ->
  {fun Fun:execute/2, 2};
normailze(Fun) when is_function(Fun) ->
  {arity, Arity} = erlang:fun_info(Fun, arity),
  {Fun, Arity};
normailze({Fun, Arity}) when is_function(Fun) andalso is_integer(Arity) ->
  {Fun, Arity};
normailze({Module, Fun, Arity}) ->
  {fun Module:Fun/Arity, Arity};
normailze({Module, Fun}) ->
  {fun Module:Fun/2, 2}.
