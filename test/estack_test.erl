-module(estack_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  Stack = [
    fun(_Req, _Env) ->
      ?debugMsg("simple: 1 start"),
      {ok, _Req, _Env}
    end,
    fun(_Req, _Env) ->
      ?debugMsg("simple: 2 start"),
      {ok, _Req, _Env}
    end
  ],
  estack:execute({}, [], Stack).

nested_test() ->
  Stack = [
    fun(_Req, _Env) ->
      ?debugMsg("nested: 1 start"),
      {ok, _Req, _Env}
    end,
    fun(_Req, _Env, Stack) ->
      ?debugMsg("nested: 2 start"),
      {ok, _Req2, _Env2} = estack:execute(_Req, _Env, Stack),
      ?debugMsg("nested: 2 end"),
      {ok, _Req2, _Env2}
    end,
    fun(_Req, _Env) ->
      ?debugMsg("nested: 3 start"),
      {ok, _Req, _Env}
    end,
    fun(_Req, _Env, Stack) ->
      ?debugMsg("nested: 4 start"),
      {ok, _Req2, _Env2} = estack:execute(_Req, _Env, Stack),
      ?debugMsg("nested: 4 end"),
      {ok, _Req, _Env}
    end,
    fun(_Req, _Env) ->
      ?debugMsg("nested: 5 start"),
      {ok, _Req, _Env}
    end
  ],
  {ok, _, _} = estack:execute({}, [], Stack).

halt_test() ->
  Stack = [
    fun(_Req, _Env) ->
      ?debugMsg("halt: 1 start"),
      {halt, _Req}
    end,
    fun(_Req, _Env) ->
      ?debugMsg("halt: 1 start"),
      {ok, _Req, _Env}
    end
  ],
  {halt, _} = estack:execute({}, [], Stack).


nested_halt_test() ->
  Stack = [
    fun(_Req, _Env) ->
      ?debugMsg("nested-halt: 1 start"),
      {ok, _Req, _Env}
    end,
    fun(_Req, _Env, Stack) ->
      ?debugMsg("nested-halt: 2 start"),
      {ok, _Req2, _Env2} = estack:execute(_Req, _Env, Stack),
      ?debugMsg("nested-halt: 2 end"),
      {ok, _Req2, _Env2}
    end,
    fun(_Req, _Env) ->
      ?debugMsg("nested-halt: 3 start"),
      {ok, _Req, _Env}
    end,
    fun(_Req, _Env, Stack) ->
      ?debugMsg("nested-halt: 4 start"),
      {ok, _Req2, _Env2} = estack:execute(_Req, _Env, Stack),
      ?debugMsg("nested-halt: 4 end"),
      {ok, _Req, _Env}
    end,
    fun(_Req, _Env) ->
      ?debugMsg("nested-halt: 5 start"),
      {halt, _Req}
    end
  ],
  {halt, _} = estack:execute({}, [], Stack).

timer_test() ->
  Stack = [
    fun(_Req, _Env) ->
      ?debugMsg("timer: 1 start"),
      {ok, _Req, _Env}
    end,
    fun(_Req, _Env, Stack) ->
      ?debugMsg("timer: 2 start"),
      {Time, {ok, _Req2, _Env2}} = timer:tc(estack, execute, [_Req, _Env, Stack]),
      ?debugMsg("timer: 2 end"),
      ?debugVal(Time),
      {ok, _Req2, _Env2}
    end,
    fun(_Req, _Env) ->
      ?debugMsg("timer: 3 start"),
      {ok, _Req, _Env}
    end,
    fun(_Req, _Env, Stack) ->
      ?debugMsg("timer: 4 start"),
      {ok, _Req2, _Env2} = estack:execute(_Req, _Env, Stack),
      ?debugMsg("timer: 4 end"),
      {ok, _Req, _Env}
    end
  ],
  {ok, _, _} = estack:execute({}, [], Stack).

memoize_test() ->
  Expected = <<"memoization is cool">>,
  Stack = [
    fun(_Req, _Env) ->
      ?debugMsg("memoize: 1 start"),
      {ok, _Req, _Env}
    end,
    fun(Req, Env, Stack) ->
      ?debugMsg("memoize: 2 start"),
      case get(Req) of
        undefined ->
          {ok, Req2, Env2} = estack:execute(Req, Env, Stack),
          ?debugMsg("memoize: 2 end"),
          put(Req, Req2),
          {ok, Req2, Env2};
        Val ->
          ?debugMsg("memoize: 2 end (memoized)"),
          {ok, Val, Env}
      end
    end,
    fun(_Req, _Env) ->
      ?debugMsg("memoize: 3 start"),
      {ok, _Req, _Env}
    end,
    fun(_Req, _Env, Stack) ->
      ?debugMsg("memoize: 4 start"),
      {ok, _Req2, _Env2} = estack:execute(_Req, _Env, Stack),
      ?debugMsg("memoize: 4 end"),
      {ok, _Req2, _Env2}
    end,
    fun(_Req, _Env) ->
      ?debugMsg("memoize: 5 start"),
      {ok, Expected, _Env}
    end
  ],
  {ok, Expected, _} = estack:execute(req, [], Stack),
  {ok, Expected, _} = estack:execute(req, [], Stack).
