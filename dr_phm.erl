-module(dr_phm).

-export(
  [ set_trace/0
  , start_trace/0
  , stop_trace/1
  , capture_udp_to_log/0
  , capture_udp_to_log/2
  , display_log/1
  , write_log_to_file/2
  , load_log_into_map/1
  , indent_trace_map/2
  , indent_trace_map/3 ] ).

% tests
-export(
  [ test/0
  , loop/0
  , iteration/0
  , iteration/1
  , round_robin/0
  , round_robin/1 ] ).

set_trace() ->
  Applications
   = [ "blackboard"
     , "elogger"
     , "fpsutils"
     , "imi"
     , "ips_log"
     , "ips_rpc"
     , "stdutils"
     , "workflow" ],
  trace_modules(
    modules_of_applications(
      Applications ) ).

modules_of_applications(Applications) ->
  %
  % NOTE: Assumed the directory name matches the application
  %       name, i.e. no version suffix.
  %
  [ erlang:list_to_existing_atom(Mod)
    || {Mod,File,_} <- code:all_available()
     , is_list(File)
     , App <- Applications
     , A <- filename:split(File)
     , A == App ].

trace_modules(Modules) ->
  Matchspec = [{'_', [], [{exception_trace}]}],
  Pattern_opt = [ local ],
  lists:sum([
    erlang:trace_pattern({M,'_','_'},Matchspec,Pattern_opt)
      || M <- Modules ]).

as_binary(In) -> erlang:term_to_binary(In).

abridge(Trace) when element(1,Trace) =:= trace; element(1,Trace) =:= trace_ts ->
  [Tag,Object,Event|Rest] = erlang:tuple_to_list(Trace),
  Short = [Tag,Object,Event|abridge_each(Event,Rest)],
  as_binary(erlang:list_to_tuple(Short));
abridge(Other) -> as_binary(Other).

abridge_each(Event,[_Msg|Rest])
when Event =:= send
   ; Event =:= send_to_non_existing_process -> [redacted|Rest];
abridge_each('receive',[_Msg|Rest]) -> [redacted|Rest];
abridge_each(call,[{Mod,Fun,Arg}|Rest]) -> [{Mod,Fun,{redacted,length(Arg)}}|Rest];
abridge_each(return_from,[MFArity,_Result|Rest]) -> [MFArity,redacted|Rest];
abridge_each(exception_from,[MFArity,_Result|Rest]) -> [MFArity,redacted|Rest];
abridge_each(spawn,[Child,{Mod,Fun,Arg}|Rest]) -> [Child,{Mod,Fun,{redacted,length(Arg)}}|Rest];
abridge_each(spawned,[Parent,{Mod,Fun,Arg}|Rest]) -> [Parent,{Mod,Fun,{redacted,length(Arg)}}|Rest];
abridge_each(exit,[_Reason|Rest]) -> [redacted|Rest];
abridge_each(Event,[_Info|Rest])
when Event =:= gc_minor_start
   ; Event =:= gc_max_heap_size
   ; Event =:= gc_minor_end
   ; Event =:= gc_major_start
   ; Event =:= gc_major_end -> [redacted|Rest];
abridge_each(_,Keep) -> Keep.

emit(Trace,Emit) ->
  case Emit(as_binary(Trace))
    of {error,emsgsize} -> Emit(abridge(Trace))
     ; ok -> ok end.


tracer(Port) ->
  {ok,Socket} = gen_udp:open(0,[binary,{active,true}]),
  Emit = fun(Binary) -> gen_udp:send( Socket, {127,0,0,1}, Port, Binary) end,
  Send = fun
    F() -> receive stop
           -> error_logger:info_msg("tracer stopped.~n",[])
            , exit(normal)
         ; Trace
           when element(1,Trace) =:= trace_ts
              ; element(1,Trace) =:= trace
           -> exit(Trace,Emit), F()
         ; _ -> F()
         after 10000
           -> error_logger:info_msg("tracer idle...~n",[])
            , F() end end,
  erlang:spawn(Send).

start_trace() ->
  Tracer = tracer(9000),
  {Tracer,start_trace(Tracer)}.

start_trace(Tracer) ->
  Trace_opt =
    [ call
    , return_to
    , timestamp
    , {tracer,Tracer} ],
  erlang:trace(all,true,Trace_opt).

stop_trace({Tracer,_}) ->
  erlang:trace(all,false,[]),
  Tracer ! stop.


recv(Socket,Log) ->
  receive stop ->
      ok = gen_udp:close(Socket),
      error_logger:info_msg("Stopped.~n",[]);
    truncate ->
      ok = disk_log:truncate(Log),
      recv(Socket,Log);
    {udp,_,_,_,Packet} when is_binary(Packet) ->
      ok = disk_log:balog(Log,Packet),
      recv(Socket,Log);
    _ ->
      recv(Socket,Log)
  after 10000 ->
    error_logger:info_msg("idle...~n",[]),
    recv(Socket,Log) end.

capture_udp_to_log() ->
  capture_udp_to_log(9000,"/tmp/trace_logger").

capture_udp_to_log(Port,Wrap_log) ->
  Args = [ {name,Wrap_log},
           {size,{8*1024*1024,8}},
           {type,wrap} ],
  Recv = fun () ->
    case disk_log:open(Args) of
      {ok,Log} ->
        error_logger:info_msg(
          "ready Port = ~p, Wrap_log = ~p.~n",
          [Port,Wrap_log] );
      {repaired,Log,_,_} = Ok ->
        error_logger:info_msg(
          "repaired ~p.~n",[Ok] ) end,
    {ok,Socket} = gen_udp:open(Port,[binary,{active,true}]),
    recv(Socket,Log) end,
  erlang:spawn(Recv).


open_log(Wrap_log,Store) ->
  {ok,Read} = wrap_log_reader:open(Wrap_log),
  First = wrap_log_reader:chunk(Read),
  read_log(First,Store).

read_log({error,_} = Bad,_) ->
  Bad;
read_log({Cont,Last,Badbytes},Store) ->
  error_logger:info_msg("Badbytes = ~p.~n",[Badbytes]),
  read_log(Cont,Last,Store);
read_log({Cont,Last},Store) ->
  read_log(Cont,Last,Store).

read_log(Cont,Last,Store) ->
  try Store(Last) of
    Fn when is_function(Fn) ->
      case wrap_log_reader:chunk(Cont) of
        {Close,eof} -> wrap_log_reader:close(Close), Fn(eof);
        Next -> read_log(Next,Fn) end;
    Out -> Out
  catch What:Why:Where ->
    Bad = {What,Why,Where},
    error_logger:warning_msg("Bad = ~p.~n",[Bad]),
    Bad end.

write_log_to_file(Wrap_log,Flat_file) ->
  {ok,Write} = file:open(Flat_file,[write,compressed]),
  Each = fun (X) -> io:format(Write,"~p.~n",[X]) end,
  Store =
    fun Fn(eof) -> ok
      ; Fn(In) when is_list(In) -> lists:foreach(Each,In), Fn end,
  open_log(Wrap_log,Store).

display_log(Wrap_log) ->
  Each = fun (X) -> io:format("~p.~n",[X]) end,
  Store =
    fun Fn(eof) -> ok;
        Fn(In) when is_list(In) -> lists:foreach(Each,In), Fn end,
  open_log(Wrap_log,Store).



store_map(Out,[]) -> fun (Next) -> store_map(Out,Next) end;
store_map(In,[Trace|Rest]) ->
  case inspect_trace(Trace) of
    ignore -> store_map(In,Rest);
    not_trace -> store_map(In,Rest);
    #{ object := Object } = Event ->
      case In of
      #{ Object := Events } ->
        store_map(In#{ Object := [Event|Events] },Rest);
      #{ } ->
        store_map(In#{ Object => [Event] },Rest) end end;
store_map(Out,eof) -> Out;
store_map(_,Bad) -> Bad.

load_log_into_map(Wrap_log) ->
  open_log(Wrap_log,fun (In) -> store_map(#{},In) end).


inspect_trace(Trace) when is_tuple(Trace) ->
  case erlang:tuple_to_list(Trace) of
    [trace,Object,Event|Rest] ->
      Base = #{ object => Object, event => Event },
      inspect_trace_info(Base,Rest);
    [trace_ts,Object,Event|Rest] ->
      [Time|_] = lists:reverse(Rest),
      Microseconds = timestamp_to_microseconds(Time),
      Base = #{ object => Object, event => Event, time => Microseconds },
      inspect_trace_info(Base,Rest);
    _ -> not_trace end;
inspect_trace(_) -> not_trace.

arity({redacted,Len}) -> Len;
arity(Arg) when is_list(Arg) -> length(Arg).

inspect_trace_info(Base,Rest) ->
  case maps:get(event,Base) of
    Event when Event =:= send
             ; Event =:= send_to_non_existing_process ->
      [Msg,To|_] = Rest,
      Base#{ msg => Msg, to => To };
    'receive' ->
      [Msg|_] = Rest,
      Base#{ msg => Msg };
    call ->
      [{Mod,Fun,Arg}|_] = Rest,
      Base#{ mfa => {Mod,Fun,arity(Arg)},
             arg => Arg };
    return_to ->
      [MFArity|_] = Rest,
      Base#{ mfa => MFArity };
    return_from ->
      [MFArity,Result|_] = Rest,
      Base#{ mfa => MFArity,
             result => Result };
    exception_from ->
      [MFArity,Reason|_] = Rest,
      Base#{ mfa => MFArity,
             reason => Reason };
    spawn ->
      [Child,{Mod,Fun,Arg}|_] = Rest,
      Base#{ child => Child,
             entry_point => {Mod,Fun,arity(Arg)},
             arg => Arg };
    spawned ->
      [Parent,{Mod,Fun,Arg}|_] = Rest,
      Base#{ parent => Parent,
             entry_point => {Mod,Fun,arity(Arg)},
             arg => Arg };
    exit ->
      [Reason|_] = Rest,
      Base#{ reason => Reason };
    Event when Event =:= register
             ; Event =:= unregister ->
      [Reg_name|_] = Rest,
      Base#{ registered_name => Reg_name };
    Event when Event =:= gc_minor_start
             ; Event =:= gc_max_heap_size
             ; Event =:= gc_minor_end
             ; Event =:= gc_major_start
             ; Event =:= gc_major_end ->
      [Info|_] = Rest,
      Base#{ gc_info => Info };
    _ -> ignore end.



-define(million,1000000).


indent_trace_map(Trace_map,N) ->
  indent_trace_map(Trace_map,N,standard_io).

indent_trace_map(Trace_map,all,Indent_file) when is_map(Trace_map) ->
  Each = fun
    (Pid,Events,N) ->
      Trace_list = lists:reverse(Events),
      header(Indent_file,N,Pid,Trace_list),
      N + 1 end,
  maps:fold(Each,1,Trace_map);
indent_trace_map(Trace_map,N,Indent_file)
when is_map(Trace_map), is_integer(N) ->
  Pid = lists:nth(N,lists:sort(maps:keys(Trace_map))),
  Trace_list = lists:reverse(maps:get(Pid,Trace_map)),
  header(Indent_file,N,Pid,Trace_list).

header(Indent_file,N,Pid,Trace_list) ->
  File_name = file_name(Indent_file,[ $- | pad(N) ]),
  indent_stack(
    [ "File_name = " ++ File_name,
      "Pid = " ++ erlang:pid_to_list(Pid) | Trace_list ],
    File_name).

file_name([],Suffix) -> Suffix ++ ".gz";
file_name(".gz",Suffix) -> Suffix ++ ".gz";
file_name([H|T],Suffix) -> [H|file_name(T,Suffix)].

pad(N) when N < 10 -> "000" ++ integer_to_list(N);
pad(N) when N < 100 -> "00" ++ integer_to_list(N);
pad(N) when N < 1000 -> "0" ++ integer_to_list(N);
pad(N) -> integer_to_list(N).

indent_stack(Trace_list,standard_io) when is_list(Trace_list) ->
  show_stack(standard_io,Trace_list);
indent_stack(Trace_list,Indent_file)
when is_list(Trace_list), is_list(Indent_file) ->
  {ok,Write} = file:open(Indent_file,[write,compressed]),
  show_stack(Write,Trace_list).

show_stack(Write,Trace_list) ->
  [Event|Rest] = Trace_list,
  case Event of
    #{ event := call } ->
      Push = [Event],
      write_event(Write,length(Push),Event),
      show_stack(Write,Push,Rest);
    _ ->
      write_other(Write,0,Event),
      show_stack(Write,Rest) end.

show_stack(standard_io,_,[]) -> ok;
show_stack(Write,_,[]) -> file:close(Write);
show_stack(Write,Stack,[Event|Trace]) ->
  case Event of
    #{ event := call } ->
      Push = [Event|Stack],
      write_event(Write,length(Push),Event),
      show_stack(Write,Push,Trace);
    #{ event := return_to, mfa := MFArity } ->
      case Stack of
        [#{ event := call, mfa := MFArity }|_] ->
          show_stack(Write,Stack,Trace);
        _ ->
          write_event(Write,length(Stack),Event),
          show_stack(Write,Stack,Trace) end;
    #{ event := E, mfa := MFArity }
    when E =:= return_from; E =:= exception_from ->
      %
      % case clause exception will display Event as well as Stack
      %
      case {Event,Stack} of
        {_,[]} ->
          %
          % Unwinding the call stack where the trace has not
          % recorded entering the function that return here.
          %
          write_event(Write,length(Stack),Event),
          show_stack(Write,[],Trace);
        {_,[#{ event := call, mfa := MFArity }|Pop]} ->
          write_event(Write,length(Stack),Event),
          show_stack(Write,Pop,Trace);
        {_,_} ->
          %
          % calls to inlined functions are not captured, but
          % the 'return_from' event is!
          %
          write_event(Write,length(Stack),
            Event#{ comment => "inlined, no 'call' event" } ),
          show_stack(Write,Stack,Trace) end;
    _ ->
      write_other(Write,length(Stack),Event) end.


indent(0) -> [];
indent(N) when N > 0 -> "  " ++ indent(N - 1).

write_other(Write,Indent,Other) ->
  Is_string = io_lib:printable_list(Other),
  if Is_string -> String = io_lib:print(Other);
     true -> String = io_lob:write(Other) end,
  io:put_chars(Write,[indent(Indent),"% ",String]),
  io:nl(Write).

write_event(Write,Indent,Event) ->
  io:put_chars(Write,[indent(Indent),fmt_event(Event)]),
  io:nl(Write).

fmt_event(Event) ->
  case Event of
    #{ event := call } ->
      fmt_call(Event);
    #{ event := exception_from, reason := Reason, mfa := MFA } ->
      [ "% exception_from ", io_lib:write(Reason),
        ". % ", io_lib:write(MFA) | comment(Event) ];
    #{ event := return_from, result := Result, mfa := MFA } ->
      [ "-> ", io_lib:write(Result),
        ". % ", io_lib:write(MFA) | comment(Event) ];
    #{ event := return_to, mfa := undefined } ->
      [ "% return_to undefined" ];
    #{ event := return_to, mfa := {Mod,Fun,Len} } ->
      [ "% return_to ", atom_to_list(Mod), $:,
        atom_to_list(Fun), $/, integer_to_list(Len) ] end.

fmt_call(#{ event := call, mfa := {Mod,Fun,Len}, arg := {redacted,Len} }) ->
  [ atom_to_list(Mod), $:,
    atom_to_list(Fun), $/, integer_to_list(Len), ". % redacted" ];
fmt_call(#{ event := call, mfa := {Mod,Fun,Len}, arg := Arg })
when Len == length(Arg) ->
  [ atom_to_list(Mod), $:,
    atom_to_list(Fun), $(, fmt_arg(Arg) ].

fmt_arg([]) -> ").";
fmt_arg([Term]) ->
  [ io_lib:write(Term), $), $. ];
fmt_arg([Term|Rest]) ->
  [ io_lib:write(Term), $, | fmt_arg(Rest) ].

comment(#{ comment := Comment }) -> [ $\s, $(, Comment, $) ];
comment(#{ }) ->[].

timestamp_to_microseconds({Mega,Unit,Micro}) ->
  ( Mega * ?million + Unit ) * ?million + Micro.


%%%
%%%   TESTS
%%%

test() ->
  {ok,Socket} = gen_udp:open(0,[binary,{active,true}]),
  Emit = fun(Binary) -> gen_udp:send( Socket,{127,0,0,1}, 9000, Binary) end,
  Send = fun F()
    -> receive stop -> exit(normal)
       ; Trace
         when element(1,Trace) =:= trace_ts
            ; element(1,Trace) =:= trace
         -> emit(Trace,Emit), F()
       ; _ -> F() end end,

  Tracer = spawn(Send),
  Trace_opt =
    [ call
    , return_to
    , timestamp
    , {tracer,Tracer} ],
  Pattern_opt = [ local ],
  Matchspec = [{'_',[],[{exception_trace}]}],

  erlang:trace_pattern({?MODULE,'_','_'},Matchspec,Pattern_opt),
  erlang:trace(all,true,Trace_opt),

  Pids = [ spawn(F)
         || F <- [ fun loop/0,
                   fun iteration/0,
                   fun round_robin/0 ] ],

  receive after 20000 ->
    _ = [ exit(P,normal) || P <- Pids ],
    erlang:trace(all,false,Trace_opt) end.

loop() -> receive after 700 -> loop() end.

iteration() -> iteration(15).

iteration(Repeat) ->
  case Repeat - 1 of
    0 -> done();
    Next -> receive after 1100 -> iteration(Next) end end.

round_robin() -> round_robin(infinity).

round_robin(Repeat) -> round_robin(Repeat,500).

round_robin(Repeat,Pause) -> rr_first(Repeat,Pause).

rr_first(Repeat,Pause) ->
  receive after Pause -> rr_second(Repeat,Pause) end.

rr_second(Repeat,Pause) ->
  receive after Pause -> rr_third(Repeat,Pause) end.

rr_third(infinity,Pause) ->
  receive after Pause -> rr_first(infinity,Pause) end;
rr_third(Repeat,Pause) ->
  case Repeat - 1 of
    0 -> done();
    Next -> receive after Pause -> rr_first(Next,Pause) end end.

done() -> io:write("done.").

% On the logging Erlang node:
%
% Cap = dr_phm:capture_udp_to_log().
%
% On the testing Erlang node:
%
% dr_phm:test().
%
% Test = dr_phm:load_log_into_map("/tmp/trace_logger.LOG").
% dr_phm:indent_trace_map(Test,1,"/tmp/pretty_trace").
% dr_phm:indent_trace_map(Test,2,"/tmp/pretty_trace").
% dr_phm:indent_trace_map(Test,3,"/tmp/pretty_trace").
% dr_phm:indent_trace_map(Test,4,"/tmp/pretty_trace").
