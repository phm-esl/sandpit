-module(make_message).

-export(
 [ from_json/1
 , from_request/1
 , test/0 ] ).

-define(log(F,A),io:format("~p:~p:~p~n\t"++F,[?MODULE,?FUNCTION_NAME,?LINE|A])).




from_json(In) when is_binary(In) ->
  Request = codec_JSON:decode(In),
  from_request(Request).

valid_req(In) ->
  maps:fold(fun valid_req/3,#{},In).

valid_req(K,V,O) when is_map(O) ->
  if K =:= base -> valid_base(V,O);
     K =:= <<"base">> -> valid_base(V,O);
     K =:= contents -> valid_contents(V,O);
     K =:= <<"contents">> -> valid_contents(V,O);
     true -> {bad_request,K} end;
valid_req(_,_,Bad) -> Bad.

valid_base(V,O) ->
  if is_list(V) -> O#{ base => V };
     is_binary(V) -> O#{ base => erlang:binary_to_list(V) };
     true -> {bad_base,V} end.

valid_contents(V,O) ->
  if is_list(V) -> O#{ contents => V };
     is_tuple(V) -> O#{ contents => V };
     true -> {bad_contents,V} end.

from_request( Request ) ->
  case valid_req(Request) of
    #{ base := Base, contents := Contents } ->
      ?log("Request = ~p.~n",[Request]),
      {ok,Paths} = paths_to_extract_map(Contents),
      ?log("Paths = ~p.~n",[Paths]),
      case generate(Base,Paths) of
        Good when is_binary(Good) -> {ok,Good};
        Bad -> {error,Bad} end;
    Bad -> {error,Bad} end.





paths_to_extract_map(Contents) when is_list(Contents) ->
  paths_to_extract_map(Contents,#{});
paths_to_extract_map(Contents) when is_tuple(Contents) ->
  paths_to_extract_map(Contents,1,#{}).

paths_to_extract_map([],Out) -> {ok,Out};
paths_to_extract_map([Each|Rest],Old) ->
  case each_path_value(Each,Old) of
    New when is_map(New) ->
      paths_to_extract_map(Rest,New);
    Bad -> Bad end.

paths_to_extract_map(C,N,Old) when N =< tuple_size(C) ->
  case each_path_value(erlang:element(N,C),Old) of
    New when is_map(New) ->
      paths_to_extract_map(C,N + 1,New);
    Bad -> Bad end;
paths_to_extract_map(_,_,Out) -> {ok,Out}.

each_path_value(In,Out) ->
  case In of
    #{ path := Path, value := Value } ->
      path_segments(split(Path,[<<$/>>]),Value,Out);
    Bad -> {bad_path,Bad} end.


split(Path,Chop) when is_binary(Path) ->
  binary:split(Path,Chop,[global]);
split(Path,Chop) when is_list(Path) ->
  split(erlang:list_to_binary(Path),Chop).


% TODO: [<<>>,<<>>|Segments] when the path prefix is //
path_segments([<<>>|Segments],Value,Out) ->
  each_path_segment(Segments,Value,Out);
path_segments([[]|Segments],Value,Out) ->
  each_path_segment(Segments,Value,Out);
path_segments(Bad,_,_) ->
  {bad_path_segments,Bad}.

each_path_segment(Segments,Value,Out) when is_binary(Value) ->
  case Segments of
%    [Last,<< $@, Attr/binary >>] ->
      %
      % TODO: /top/middle/bottom/@name == path to
      %       <top><middle><bottom "name"="HERE" /></middle></top>
      %
    [Last] when is_binary(Last) ->
      %
      % This is a predicate test, not the path of an attribute.
      %
      case binary:split(Last,[<<"[@">>,<<$]>>],[global]) of
        [Name,Attr,<<>>] ->
          Out#{ {xpath,Name} => #{ attr => #{ Attr => Value } } };
        [Name] ->
          Out#{ Name => [Value] } end;
    [Name|Rest] ->
      case Out of
        #{ Name := Old } when is_map(Old) ->
          New = each_path_segment(Rest,Value,Old),
          Out#{ Name := New };
        #{ } ->
          New = each_path_segment(Rest,Value,#{}),
          Out#{ Name => New } end end;
each_path_segment(Segments,Value,Out) ->
  each_path_segment(Segments,erlang:list_to_binary(Value),Out).


%%%
%%%   Assume that all messages are XML generated from XSD.
%%%
generate(Base,Paths) when is_binary(Base) ->
  generate(erlang:binary_to_list(Base),Paths);
generate(Name,Paths) ->
  Dir = "protocol_tools/priv/",
  case filelib:wildcard(Name,Dir) of
    [] -> {not_found,Name};
    [_,_|_] -> {not_unique,Name};
    [File] ->
      File_name = Dir ++ File,
      Options = [{insertions,Paths},minimal],
      From_xsd = schema_xsd:generate_from_XSD_file(File_name,Options),
      ?log("From_xsd = ~p.~n",[From_xsd]),
      ok = file:write_file("original.xml",codec_xml:encode(From_xsd)),
      codec_xml:encode( From_xsd, Paths ) end.



test() ->
  test_1() andalso test_2().

test_1() ->
  {ok,Request} = file:read_file("protocol_tools/priv/request.json"),
  {ok,Message} = make_message:from_json(Request),
  ?log("Message = ~p.~n",[Message]),
  ok =:= file:write_file("modified.xml",Message).

test_2() ->
  Data_file = "protocol_tools/priv/request-make_message.erlang",
  {ok,[Request]} = file:consult(Data_file),
  {ok,Message} = make_message:from_request(Request),
  ?log("Message = ~p.~n",[Message]),
  ok =:= file:write_file("modified.xml",Message).
