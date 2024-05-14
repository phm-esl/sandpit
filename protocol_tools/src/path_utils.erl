-module(path_utils).

-export(
 [ extraction_paths/1
 , rollup/1 ]).


tree_from_map(In) ->
  maps:fold(
    fun (K,V,O) -> [{K,tree_from_map(V)}|O] end, [], In).

extraction_paths(Paths) when is_map(Paths) ->
  case tree_from_map(Paths) of
    [Tree] -> extraction_paths(Tree);
    [] -> [] end;
extraction_paths(Paths) when is_tuple(Paths) ->
  lists:foldl(
    fun (I,O) -> O#{ lists:reverse(I) => [] } end,
    #{},
    unroll(Paths) ).

unroll({Atom,[]}) -> [[Atom]];
unroll({Atom,List}) -> [ [Atom|U] || L <- List, U <- unroll(L) ].


rollup(In) when is_map(In) ->
  Rev_keys = maps:fold(fun rev_keys/3,[],In),
  lists:foldl(fun to_tree/2,#{},Rev_keys).

rev_keys(Key,Value,Out) -> [{lists:reverse(Key),Value}|Out].

to_tree({[Last],Value},Out) when not is_map_key(Last,Out) ->
  Out#{ Last => Value };
to_tree({[Head|Tail],Value},Out) when is_map_key(Head,Out)  ->
  Exist = maps:get(Head,Out),
  Out#{ Head := to_tree({Tail,Value},Exist) };
to_tree({[Head|Tail],Value},Out) when not is_map_key(Head,Out)  ->
  Out#{ Head => to_tree({Tail,Value},#{}) }.
