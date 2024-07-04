-module(make_message).

-export(
 [ from_json/1
 , from_request/1
 , test/0 ] ).

-define(log(F,A),io:format("~p:~p:~p~n\t"++F,[?MODULE,?FUNCTION_NAME,?LINE|A])).




from_json(In) when is_binary(In) ->
  Request = codec_JSON:decode(In),
  from_request(Request).

from_request( #{ base := Base, <<"contents">> := Contents } = Request ) ->
  ?log("Request = ~p.~n",[Request]),
  Paths = paths_to_extract_map(Contents),
  ?log("Paths = ~p.~n",[Paths]),
  case generate(Base,Paths) of
    Good when is_binary(Good) -> {ok,Good};
    Bad -> {error,Bad} end.





paths_to_extract_map(Contents) when is_tuple(Contents) ->
  paths_to_extract_map(Contents,1,#{}).

paths_to_extract_map(C,N,Old) when N =< tuple_size(C) ->
  #{ path := Path, value := Value } = erlang:element(N,C),
  case binary:split(Path,[<<$/>>],[global]) of
    % TODO: [<<>>,<<>>|Segments] when the path prefix is //
    [<<>>|Segments] ->
      New = each_path_segment(Segments,Value,Old),
      paths_to_extract_map(C,N + 1,New) end;
paths_to_extract_map(_,_,Out) -> Out.

each_path_segment(Segments,Value,Out) ->
  case Segments of
    [Last] ->
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
          Out#{ Name => New } end end.

%%%
%%%   Assume that all messages are XML generated from XSD.
%%%
generate(Base,Paths) when is_binary(Base) ->
  generate(erlang:binary_to_list(Base),Paths);
generate(Name,Paths) ->
  Dir = "protocol_tools/priv/",
  case filelib:wildcard(Name,Dir) of
    [] -> not_found;
    [_,_|_] -> not_unique;
    [File] ->
      File_name = Dir ++ File,
      Options = [{insertions,Paths},minimal],
      From_xsd = schema_xsd:generate_from_XSD_file(File_name,Options),
      ?log("From_xsd = ~p.~n",[From_xsd]),
      ok = file:write_file("original.xml",codec_xml:encode(From_xsd)),
      codec_xml:encode( From_xsd, Paths ) end.



test() ->
  {ok,Request} = file:read_file("protocol_tools/priv/request.json"),
  {ok,Message} = make_message:from_json(Request),
  ?log("Message = ~p.~n",[Message]),
  file:write_file("modified.xml",Message).
