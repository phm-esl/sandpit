-module(make_message).

-export(
 [ from_json/1
 , from_request/1
 , test/0 ] ).

-define(log(F,A),io:format("~p:~p:~p~n\t"++F,[?MODULE,?FUNCTION_NAME,?LINE|A])).




from_json(In) when is_binary(In) ->
  Request = codec_JSON:decode(In),
  from_request(Request).

from_request( #{ base := Base, <<"contents">> := Contents } ) ->
  Paths = paths_to_extract_map(Contents),
  ?log("Paths = ~p.~n",[Paths]),
  generate(Base,Paths).





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
          Key = to_atom(Name),
          Out#{ {xpath,Key} => #{ attr => #{ Attr => Value } } };
        [Name] ->
          Key = to_atom(Name),
          Out#{ Key => [Value] } end;
    [Name|Rest] ->
      Key = to_atom(Name),
      case Out of
        #{ Key := Old } when is_map(Old) ->
          New = each_path_segment(Rest,Value,Old),
          Out#{ Key := New };
        #{ } ->
          New = each_path_segment(Rest,Value,#{}),
          Out#{ Key => New } end end.

to_atom(Atom) when is_atom(Atom) -> Atom;
to_atom(Binary) when is_binary(Binary) ->
  %% TODO: avoid the converstion to atom, change the generator to handle
  %%       element names as binaries exclusively.
  erlang:binary_to_atom(Binary).
%  Binary.

%%%
%%%   Assume that all messages are XML generated from XSD.
%%%
generate(Base,Paths) when is_binary(Base) ->
  generate(erlang:binary_to_list(Base),Paths);
generate(Name,Paths) ->
  Dir = "protocol_tools/priv/",
  [File] = filelib:wildcard(Name ++ ".*.[Xx][Ss][Dd]",Dir),
  File_name = Dir ++ File,
  Options = [{insertions,Paths},minimal],
  From_xsd = schema_xsd:generate_from_XSD_file(File_name,Options),
  ?log("From_xsd = ~p.~n",[From_xsd]),
  generate_loop( codec_xml:encode( From_xsd, Paths ) ).


generate_loop(In) ->
  case In of
    {Fn,_Original,Insertion} when is_function(Fn) ->
      ?log("Insertion = ~p.~n",[Insertion]),
      {_,Inject,Attr} = hd(Insertion),
      Update = {Inject,Attr},
      generate_loop( Fn( Update ) );
    Out -> Out end.




test() ->
  {ok,Request} = file:read_file("protocol_tools/priv/request.json"),
  Message = make_message:from_json(Request),
  ?log("Message = ~p.~n",[Message]),
  file:write_file("tmp.xml",Message).
