-module(coverage).

-export([run_test/1,to_file/1]).

run_test(Module) ->
  Output = fun () -> cover:analyse(Module) end,
  {ok,Analysis} = analyse_module(Module,Output),
  [ {{M,F,A},Hits,Miss} || {{M,F,A},{Hits,Miss}}
                    <- Analysis,
                       M =:= Module,
                       0 < Miss ].

analyse_module(Module,Output) ->
  [Source] = [ S || {source,S} <- Module:module_info(compile) ],
  {ok,Module} = cover:compile(Source),
  ok = Module:test(),
  Result = Output(),
  cover:stop(),
  Result.

to_file(Module) ->
  Output = fun () ->
     cover:analyse_to_file(Module,[{outfile,"coverage.out"}]) end,
  {ok,_} = analyse_module(Module,Output).