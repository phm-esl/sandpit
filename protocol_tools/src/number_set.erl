-module(number_set).

-export([test/0]).

-export(
 [ insert/2
 , remove/2 ] ).

-define(log(F,A),io:format("~p:~p:~p~n\t"++F,[?MODULE,?FUNCTION_NAME,?LINE|A])).

insert([],Set) -> Set;
insert([Head|Tail],Set) -> insert(Tail,insert(Head,Set));
insert(Nbr,Set) when is_integer(Nbr) ->
  insert({Nbr,Nbr + 1},Set);
insert({Min,Max},Set)
when is_integer(Min), is_integer(Max), is_list(Set), Min < Max ->
  insert_scan(Min,Max,Set).

insert_scan(Min,Max,[]) ->
  [{Min,Max}]; % highest
insert_scan(Min,Max,[{Low,_}|_] = Set) when Max < Low ->
  [{Min,Max}|Set]; % lowest
insert_scan(Min,Max,[{Low,High}|Set]) when High < Min ->
  [{Low,High}|insert_scan(Min,Max,Set)]; % continue...
insert_scan(Min,Max,[{Low,H},{L,High}|Set])
when Low < Min, Min =< H, L =< Max, Max < High ->
  [{Low,High}|Set]; % bridge two ranges
insert_scan(Min,Max,[{Low,High}|Set]) when Min =< Low, Max =< High ->
  [{Min,High}|Set]; % overlap Low only
insert_scan(Min,Max,[{Low,High}|Set]) when Low =< Min, High =< Max ->
  [{Low,Max}|Set]; % overlap High only
insert_scan(Min,Max,[{Low,High}|_] = Set) when Low =< Min, Max =< High ->
  Set; % overlap inside Low and High
insert_scan(Min,Max,[{Low,High}|Set]) when Min =< Low, High =< Max ->
  insert_scan(Min,Max,Set). % overlap outside Low and High


remove([],Set) -> Set;
remove([Head|Tail],Set) -> remove(Tail,remove(Head,Set));
remove(Nbr,Set) when is_integer(Nbr) ->
  remove({Nbr,Nbr + 1},Set);
remove({Min,Max},Set)
when is_integer(Min), is_integer(Max), is_list(Set), Min < Max ->
  remove_scan(Min,Max,Set).

remove_scan(_,_,[]) ->
  []; % last
remove_scan(_,Max,[{Low,_}|_]=Set) when Max < Low ->
  Set; % break-out
remove_scan(Min,Max,[{Low,High}|Set]) when High < Min ->
  [{Low,High}|remove_scan(Min,Max,Set)];
remove_scan(Min,Max,[{Low,High}|Set]) when Min =< Low, High =< Max ->
  remove_scan(Min,Max,Set); % overlap outside Low and High
remove_scan(Min,Max,[{Low,High}|Set]) when Low < Min, Max < High ->
  [{Low,Min},{Max,High}|Set]; % overlap inside Low and High, split range
remove_scan(Min,Max,[{Low,High}|Set]) when Min =< Low, Low =< Max ->
  [{Max,High}|Set]; % overlap Low only
remove_scan(Min,Max,[{Low,High}|Set]) when Min =< High, High =< Max ->
  [{Low,Min}|remove_scan(Min,Max,Set)]. % overlap High only



is_valid([]) -> true;
is_valid([{A,B}]) -> A < B;
is_valid([{A,B},{C,D}|Set]) ->
  A < B andalso B < C andalso C < D andalso is_valid([{C,D}|Set]).

test() ->
  [{5,6}] = insert(5,[]), % highest
  [{5,6}] = insert(5,[{5,6}]),
  [{1,2},{5,6}] = insert({1,2},[{5,6}]), % lowest
  [{1,2},{3,4},{5,6}] = insert(3,[{1,2},{5,6}]), % continue, lowest
  [{1,6},{8,9}] = insert(8,[{1,6}]), % continue, highest
  [{1,9}] = insert(5,[{1,5},{6,9}]), % bridge two ranges
  [{1,9}] = insert({1,7},[{5,9}]), % overlap Low only
  [{1,9}] = insert({5,9},[{1,7}]), % overlap High only
  [{1,9}] = insert(5,[{1,9}]), % overlap inside Low and High
  [{1,9}] = insert({1,9},[{2,3},{4,5}]), % overlap outside Low and High

  [] = remove(5,[]), % last
  [] = remove(5,[{5,6}]),% overlap outside Low and High
  [] = remove({4,6},[{5,6}]),% overlap outside Low and High
  [{1,2}] = remove({4,7},[{1,2},{5,6}]),% overlap outside Low and High
  [{1,2},{8,9}] = remove({4,7},[{1,2},{5,6},{8,9}]),% overlap outside Low and High
  [{7,9}] = remove({4,7},[{5,9}]), % overlap low only
  [{7,9},{10,99}] = remove({4,7},[{5,9},{10,99}]), % overlap low only
  [{1,2},{7,9},{10,99}] = remove({4,7},[{1,2},{5,9},{10,99}]), % overlap low only
  [{4,5}] = remove({5,9},[{4,7}]), % overlap High only
  [{1,5},{6,9}] = remove(5,[{1,9}]), % overlap inside Low and High, split range

  Set = test_loop(1000,[]),
  Holes = remove(Set,[{0,100}]),
  [{0,100}] = insert(Set,Holes).

test_loop(0,Set) -> Set;
test_loop(N,Set) when N > 0 ->
  Span = 4,
  Min = random_integer(0,100 - Span),
  Max = Min + random_integer(1,Span),
  Op = random_integer(0,1),
  if Op =:= 0 -> X = insert({Min,Max},Set);
     Op =:= 1 -> X = remove({Min,Max},Set) end,
  Yes = is_valid(X),
  if Yes -> test_loop(N - 1,X);
     true -> X end.

random_integer(Min,Max) -> Min + rand:uniform(Max - Min + 1) - 1.
