-module(database).

-export(
  [ build_tables/3
  , lookup/2
  , table_info/2
  , transaction_history/2

  , activity/2

  , delete_record/3
  , update_record/5
  , prev/2
  , next/2 ]).


build_tables(Tables,Sources,Info) ->
  ok = schema_check(Tables),
  [Data] = [N||{N,X}<-Tables,{record_name,data}<-X],
  [Xref] = [N||{N,X}<-Tables,{record_name,xref}<-X],
  init_sources(Sources,Info#{ data => Data, xref => Xref }).

schema_check(Needed) ->
    Info_keys =
    [ type
    , disc_copies
    , record_name
    , attributes
    , {frag_properties
      ,[hash_module
       ,n_fragments
       ,node_pool
       ,n_disc_only_copies]} ],

    {Tables,Existing} =
    try All = mnesia:system_info(tables)
      , {All,[ T || {N,_} <- Needed, T <- All, T == N ]}
    catch exit:{aborted,{node_not_running,_}} -> {[],[]} end,

    case [{Tbl,[table_info(Tbl,Inf)||Inf<-Info_keys]}||Tbl<-Existing]
      of Needed -> ok
       ; [] when Tables == [schema]; Tables == []
         -> mnesia:stop()
          , mnesia:delete_schema([node()])
          , mnesia:create_schema([node()])
          , mnesia:start()
          , ok = install_each(Needed)
       ; Other -> throw({incompatible_mnesia_schema,Other})
     end.

install_each([]) -> ok;
install_each([{Name,Args}|Rest]) ->
    case mnesia:create_table(Name,Args)
      of {atomic,ok} -> install_each(Rest)
       ; Error -> {Name,Error}
     end.

init_sources([],Out) -> Out;
init_sources([{Name,Conf}|Rest],In)
when is_atom(Name), is_list(Conf) ->
  Out = In#{ {source,Name} => each_source(Conf,#{}) },
  init_sources(Rest,Out).

each_source([],Out) -> Out;
each_source([{secondaries,Sec}|Rest],In) ->
  Out = In#{ secondaries => Sec},
  each_source(Rest,Out).


%%%
%%%  Info = #{
%%%    data :=
%%%    xref :=
%%%    {source,Source} := #{ secondaries := atom() | {atom(),[...]} } }
%%%

prev(Index,Info) ->
  #{ xref := Table } = Info,
  Fn = fun mnesia:prev/2,
  case activity(Fn,[Table,Index])
    of '$end_of_table' -> []
     ; Next -> [Next]
  end.

next(Index,Info) ->
  #{ xref := Table } = Info,
  Fn = fun mnesia:next/2,
  case activity(Fn,[Table,Index])
    of '$end_of_table' -> []
     ; Next -> [Next]
  end.

lookup(Index,Info) ->
  Fun = fun () ->
    case read_xref(Index,Info)
      of [#{ uid := Uid }] -> true
       ; [] -> Uid = Index end,
    case read_data(Uid,Info)
      of [Fields] -> Fields
       ; [] -> [] end end,
  Fields = activity(Fun,[]),
  Keys = key_fields(Fields,Info),
  [{keys,Keys},{fields,Fields}].

key_fields([],_) -> [];
key_fields([{Source,Fields}|Rest],Info) ->
  Key = {source,Source},
  case Info
    of #{ Key := Config }
       -> #{ secondaries := Secondaries} = Config
        , [{Source,each_secondary(Secondaries,Fields)}|key_fields(Rest,Info)]
     ; #{ } -> key_fields(Rest,Info) end.
  

table_info(Tbl,{Inf,Keys}) ->
    case activity(fun mnesia:table_info/2, [Tbl,Inf])
      of [] -> {Inf,[]}
       ; Props ->
            Got = [ T || K<-Keys, {T,_}<-Props, T == K ],
            More =
            [ {K,activity(fun mnesia:table_info/2,[Tbl,K])}
              || K<-Keys, not lists:member(K,Got) ],
            {Inf,[{T,V}||K<-Keys,{T,V}<-Props ++ More,K==T]}
     end;
table_info(Tbl,Inf) ->
    {Inf,activity(fun mnesia:table_info/2, [Tbl,Inf])}.

transaction_history({ok,Event},Info) when is_list(Event) ->
    case proplists:get_value(uid,Event)
      of undefined -> ok
       ; Uid
         -> Fun = fun write_history/3
          , activity(Fun,[Uid,Event,Info])
     end;
transaction_history(_,_) ->
    ok.

write_history(Uid,Event,Info) ->
  case read_data(Uid,Info)
    of [] -> ok
     ; [Data]
       -> Write = [{history,differences(Event)}|Data]
        , write_data(Uid,Write,Info)
   end.

differences(Input) ->
    Was = proplists:get_value(was,Input,[]),
    Now = proplists:get_value(now,Input,[]),
    diff(Was,Now).

diff(Was,Now) ->
    each_diff(lists:sort(proplists:unfold(Was)),
      lists:sort(proplists:unfold(Now)),[]).

each_diff([],[],_) ->
    [];
each_diff([Same|Was],[Same|Now],Out) ->
    each_diff(Was,Now,Out);

each_diff([{Same,W}|Was],[{Same,N}|Now],Out)
when is_list(W), is_tuple(hd(W))
   , is_list(N), is_tuple(hd(N)) ->
    each_diff(W,N,[Same|Out]) ++ each_diff(Was,Now,Out);
each_diff([{Same,W}|Was],[{Same,N}|Now],Out) ->
    [{lists:reverse([Same|Out]),W,N}|each_diff(Was,Now,Out)];

each_diff([W|Was],[N|_]=Now,Out) when W < N ->
    [{lists:reverse(Out),W,[]}|each_diff(Was,Now,Out)];
each_diff([W|_]=Was,[N|Now],Out) when W > N ->
    [{lists:reverse(Out),[],N}|each_diff(Was,Now,Out)];

each_diff([_|_]=Was,[],Out) ->
    [{lists:reverse(Out),Was,[]}];
each_diff([],[_|_]=Now,Out) ->
    [{lists:reverse(Out),[],Now}].



%%%
%%%   Basic Mnesia transaction activity functions.
%%%
activity(Fun,Arg) ->
    Txn = fun() ->
      try apply(Fun,Arg)
      catch What:Why:Where
        -> Pdict = erlang:get()
         , { What,Why,Where,Pdict }
      end
    end,
    mnesia:activity(async_dirty,Txn,[],mnesia_frag).

read_data(Uid,Info) when is_binary(Uid) ->
    #{ data := Table } = Info,
    case mnesia:read(Table,Uid,read)
      of [] -> []
       ; [{data,Uid,Data}] -> [Data]
     end.

write_data(Uid,Data,Info) when is_binary(Uid) ->
    #{ data := Table } = Info,
    ok == mnesia:write(Table,{data,Uid,Data},write).

delete_data(Uid,Info) when is_binary(Uid) ->
    #{ data := Table } = Info,
    ok == mnesia:delete(Table,Uid,write).


read_xref(Index,Info) ->
    #{ xref := Table } = Info,
    case mnesia:dirty_read(Table,Index)
      of [{xref,Index,Value}] -> [Value]
       ; [] -> []
     end.

write_xref([],_,_) -> true;
write_xref([Index|Rest],Value,Info) ->
  write_xref(Index,Value,Info) andalso write_xref(Rest,Value,Info);
write_xref(Index,Value,Info) ->
    #{ xref := Table } = Info,
    ok == mnesia:write(Table,{xref,Index,Value},write).

delete_xref([],_) -> true;
delete_xref([Index|Rest],Info) ->
  delete_xref(Index,Info) andalso delete_xref(Rest,Info);
delete_xref(Index,Info) ->
    #{ xref := Table } = Info,
    ok == mnesia:delete(Table,Index,write).

%%%
%%%   Compound Mnesia transaction functions.
%%%
delete_record(Source,Index,Info) ->
  case read_xref(Index,Info)
    of [#{ uid := Uid }] -> delete_fields(Uid,Source,Info)
     ; [] -> {ok,not_found} end.

update_record(Source,Index,Write,Create,Info) ->
  %%
  %%  The Index value is used when modifying an existing
  %%  record, to recover the original record contents.  It
  %%  is assumed, but not enforced, that the Index matches
  %%  one of the 'secondaries' in the Write value.
  %%
  Modified = {modified,modified()},
  Input = [Modified|Write],
  case read_xref(Index,Info)
    of [#{ uid := Uid }]
       -> update_fields(Uid,Source,Input,Info)
     ; [] when Create
       -> Uid = uuid_v4()
        , update_fields(Uid,Source,Input,Info)
     ; [] -> {ok,not_found} end.

modified() ->
  [time:zulu_time_date(os:timestamp())].

delete_fields(Uid,Source,Info) when is_binary(Uid) ->
    case read_data(Uid,Info)
      of [Data] ->
        case proplists:get_value(Source,Data)
          of undefined -> {ok,no_change}
           ; Lookup
             -> Config = maps:get({source,Source},Info)
              , Xrefs = [I||{_,I}<-secondaries(Lookup,Config)]
              , true = delete_xref(Xrefs,Info)
              , case [D||D<-Data,element(1,D) /= Source]
                  of [] -> delete_data(Uid,Info)
                   ; Write -> write_data(Uid,Write,Info)
                 end
              , {ok,[{uid,Uid},{was,Lookup},{now,[]}]}
         end
       ; [] -> {ok,not_found}
     end.

update_fields(Uid,Source,Input,Info) when is_binary(Uid) ->
    case read_data(Uid,Info)
      of [Data] ->
        case proplists:get_value(Source,Data)
          of undefined
             -> Lookup = []
              , Keep = [{T,V}||{T,V}<-Input,V /= delete]
              , Insert = [{created,proplists:get_value(modified,Keep)}|Keep]
           ; Lookup when is_list(Lookup)
             -> Delete = [T||{T,delete}<-Input]
              , Keep = [{T,V}||{T,V}<-Lookup,not lists:member(T,Delete)]
              , Replace = [{T,V}||{T,V}<-Input,V /= delete]
              , Update = Replace ++ Keep
              , Keys = proplists:get_keys(Update)
              , Insert = [proplists:lookup(K,Update)||K<-Keys]
         end
       ; []
         -> Data = [{uid,Uid}]
          , Lookup = []
          , Keep = [{T,V}||{T,V}<-Input,V /= delete]
          , Insert = [{created,proplists:get_value(modified,Keep)}|Keep]
     end,
    Config = maps:get({source,Source},Info),
    Old = secondaries(Lookup,Config),
    New = secondaries(Insert,Config),
    ok = update_indexes(#{ uid => Uid },Old,New,Info),
    Other = [D||D<-Data,element(1,D) /= Source],
    Write = [{Source,Insert}|Other],
    write_data(Uid,Write,Info),
    {ok,[{uid,Uid},{was,Lookup},{now,Insert}]}.

%%%
%%%   Extract values from fields that serve as
%%%   cross-reference (xref) indexes.
%%%
secondaries(Fields,Config) ->
  #{ secondaries := Secondaries } = Config,
  each_secondary(Secondaries,Fields).

each_secondary([],_) -> [];
each_secondary([{Tag,List}|Rest],Fields) when is_atom(Tag), is_list(List) ->
  Subs = [X||{T,V}<-Fields,Tag==T,is_list(V),X<-V],
  each_secondary(List,Subs) ++ each_secondary(Rest,Fields);
each_secondary([Tag|Rest],Fields) when is_atom(Tag) ->
  Val = [trim(X)||{T,V}<-Fields,Tag==T,is_list(V),X<-V],
  [{Tag,Val}|each_secondary(Rest,Fields)].

%%%
%%%   TODO: Remove the trim/1 function.
%%%
trim(<<"07",In:9/binary>>) -> <<"447",In/binary>>;
trim(<<"07",In:9/binary,$:,_/binary>>) -> <<"447",In/binary>>;
trim(In) -> In.

update_indexes(_,[],[],_) -> ok;
update_indexes(Value,[Same|Rest_a],[Same|Rest_b],Info) ->
    update_indexes(Value,Rest_a,Rest_b,Info);
update_indexes(Value,[{Tag,Old}|Rest_a],[{Tag,New}|Rest_b],Info) ->
    Xref = Value#{ tag => Tag },
    true = no_stolen_xrefs(Xref,Tag,New,Info),
    true = delete_xref(Old,Info),
    true = write_xref(New,Xref,Info),
    update_indexes(Value,Rest_a,Rest_b,Info).

%%%
%%%   Function no_stolen_xrefs/4 will prevent stealing an xref
%%%   already in use by another record.  Updates that
%%%   modify xref values that are either unused, or that
%%%   point to the same record UUID, are permitted.
%%%
no_stolen_xrefs(_,_,[],_) -> true;
no_stolen_xrefs(Xref,Tag,[New|Rest],Info) ->
    case read_xref(New,Info)
      of [] -> no_stolen_xrefs(Xref,Tag,Rest,Info)
       ; [Xref] -> no_stolen_xrefs(Xref,Tag,Rest,Info)
       ; [_] -> throw({index_not_free,Tag,New}) end.

%%%
%%%   RFC4122, approximately :-)
%%%
%%%   The timestamp is a 60-bit value.  For UUID version 1,
%%%   this is represented by Coordinated Universal Time
%%%   (UTC) as a count of 100-nanosecond intervals since
%%%   00:00:00.00, 15 October 1582 (the date of Gregorian
%%%   reform to the Christian calendar).
%%%
%%%   100-nanosecond interval = 0.1 microseconds
%%%
uuid_v4() ->
    Rand = crypto:strong_rand_bytes(16),
    <<Low:32,Mid:16,_:4,High:12, _:2,Seq_hi:6,Seq_lo:8, Mac:48>> = Rand,
    Vsn = 4,
    Variant = 2#10, % bit pattern for value variant one.
    <<Low:32,Mid:16,Vsn:4,High:12, Variant:2,Seq_hi:6,Seq_lo:8 ,Mac:48>>.


