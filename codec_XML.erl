-module(codec_XML).

-export(
  [ open_stream/1
  , encode_stream/2
  , close_stream/1
  , encode/2
  , decode/1
  , decode/3
  , load_config/1 ] ).

-include_lib("spy/include/spy.hrl").

-type data_path()
      :: atom().

-type attrib()
      :: {binary(),binary()}
       | {binary(),data_path()}.  % tag token to be replaced by value in Data

-type node_list()
      :: [ xml_node() | attrib(), ... ].

-type node_tag()
      :: binary().

-type node_option()
      :: { 'when_empty', 'start_and_end' | 'keep_element' }
       | { 'for_each', data_path() }.

-type node_option_list()
      :: [ node_option(), ... ].

-type xml_node()
      :: atom()
       | binary()
       | {'declaration',binary()}
       | {'comment',binary()}
       | {'doctype',binary()}
       | {'CDATA',binary()}
       | { 'node'
         , node_tag()
         , node_option_list()
         , node_list() }.


-define(cr,$\r).
-define(lf,$\n).
-define(tab,$\t).
-define(sp,$\ ).
-define(whitespace,[<<$\s>>,<<$\r>>,<<$\n>>,<<$\t>>]).
-define(lt,$<).
-define(gt,$>).
-define(amp,$&).
-define(apos,$').
-define(quot,$").
-define(sol,$/).
-define(excl,$!).
-define(quest,$?).
-define(dash,$-).
-define(equals,$=).

-define(timeout,5000).


encode(Form,Fill) ->
  encode_no_stream(Form,Fill).

encode_no_stream(Form,Data) ->
  Rtn = nil,
  Nest = [],
  Head = [],
  Tail = [],
  case encode_each(Rtn,Nest,Form,Data,Head,Tail)
    of {head_tail,Rtn,Hd,Tl,Rest}
       -> Out = list_to_binary(lists:reverse(Hd,Tl))
        , if map_size(Rest) =:= 0
            -> {ok,Out}
           ; true -> {unused_values,Rest,Out} end
     ; {abort_unused_values,Rtn,Name,Hd,Tl,Rest}
       -> Bin = list_to_binary(lists:reverse(Hd,Tl))
        , {unused_values,{Name,Rest},Bin} end.


open_stream(Form) ->
  Enc =
    fun () ->
      receive {{Ref,Pid},stop}
              when is_reference(Ref), is_pid(Pid)
              -> if is_map(Form) -> First = []
                  ; is_list(Form) -> First = #{ } end
            ; {{Ref,Pid},First}
              when is_reference(Ref), is_pid(Pid)
              -> if is_map(Form) and is_list(First) -> true
                  ; is_list(Form) and is_map(First) -> true end end,
      Rtn = {Ref,Pid},
      Nest = Head = Tail = [],
      case encode_each(Rtn,Nest,Form,First,Head,Tail)
        of {head_tail,{R,P},Hd,[],Rest}
           when R =:= Ref, is_pid(P), map_size(Rest) =:= 0
           -> P ! {R,{success,list_to_binary(lists:reverse(Hd))}}
         ; {head_tail,{R,P},Hd,Tl,Rest}
           when R =:= Ref, is_pid(P), map_size(Rest) > 0
           -> Bin = list_to_binary(lists:reverse(Hd,Tl))
            , P ! {R,{unused_values,Rest,Bin}}
         %%
         %%  TODO: {abort_unused_cluster,Name}
         %%
         ; {abort_unused_values,{R,P},Name,Hd,Tl,Rest}
           when R =:= Ref, is_pid(P)
           -> Bin = list_to_binary(lists:reverse(Hd,Tl))
            , P ! {R,{unused_values,{Name,Rest},Bin}} end end,
  Ref = erlang:make_ref(),
  Pid = erlang:spawn(Enc),
  {Pid,Ref}.

encode_stream(Req,Stream) ->
  ?dbg("",[]),
  request_stream(Req,Stream).

close_stream(Stream) ->
  ?dbg("",[]),
  request_stream([],Stream).

request_stream(Req,{Pid,Ref}) when is_pid(Pid), is_reference(Ref)  ->
  Self = erlang:self(),
  Pid ! {{Ref,Self},Req},
  %%
  %%  Only set monitors when expecting a result message to
  %%  return from the spawned process.
  %%
  Monitor = erlang:monitor(process,Pid),
  receive {Ref,Out} -> true
        ; {'DOWN',Monitor,process,Pid,Why} -> Out = {error,Why}
  after 1000
    -> exit(Pid,timeout)
     , Out = {error,timeout} end,
   erlang:demonitor(Monitor,[flush]),
   Out.

encode_list(Rtn,_,[],Fill,Head,Tail) when is_map(Fill) ->
  {head_tail,Rtn,Head,Tail,Fill};
encode_list(Rtn,Nest,[Form|More],Fill,Head,Tail) when is_map(Fill) ->
  case encode_each(Rtn,Nest,Form,Fill,Head,Tail)
    of {head_tail,New,H,T,Rest}
       -> encode_list(New,Nest,More,Rest,H,T)
     ; Error -> ?dbg("Error = ~p.~n",[Error]), Error end.

encode_map(Rtn,Nest,Form,[],Head,Tail) when is_map(Form) ->
  case request_more_data(Rtn,Nest,Head)
    of {resume,New,[],Hd}
       -> {head_tail,New,Hd,Tail,#{ }} % empty
     ; {resume,New,More,Hd}
       -> encode_each(New,Nest,Form,More,Hd,Tail) end;
encode_map(Rtn,Nest,Form,[{Name,Fill}|More],Head,Tail) when is_map(Form) ->
  case Form
    of #{ Name := This }
       -> {head_tail,New,H,T,Rest} =
            encode_each(Rtn,Nest,This,Fill,Head,Tail)
        , case map_size(Rest)
            of 0 -> encode_map(New,Nest,Form,More,H,T)
             ; N when N > 0 -> {abort_unused_values,Rtn,Name,H,T,Rest} end
     ; #{ } -> {abort_unused_cluster,Name} end.

request_more_data({Ref,Pid},Nest,Head)
when is_reference(Ref), is_pid(Pid), is_list(Nest) ->
  Body = list_to_binary(lists:reverse(Head)),
  Request = {Ref,{gimme_more,Nest,Body}},
  Pid ! Request,
  receive {{Ref,New}=Rtn,[]} when is_pid(New)
          -> {resume,Rtn,[],[]}
        ; {{Ref,New}=Rtn,More} when is_pid(New), is_list(More)
          -> {resume,Rtn,More,[]} end;
request_more_data(Rtn,_,Head) -> {resume,Rtn,[],Head}.

encode_each(Rtn,Nest,Form,Fill,Head,Tail) ->
  case Form
    of List when is_list(List)
       -> encode_list(Rtn,Nest,List,Fill,Head,Tail)
     ; Map when is_map(Map)
       -> encode_map(Rtn,Nest,Map,Fill,Head,Tail)
     ; Bin when is_binary(Bin)
       -> Hd = [body_text(Bin)|Head]
        , {head_tail,Rtn,Hd,Tail,Fill}
     ; Name when is_atom(Name)
       -> case maps:take(Name,Fill)
            of error -> {head_tail,Rtn,Head,Tail,Fill}
             ; {Took,Rest}
               -> encode_each(Rtn,Nest,Took,Rest,Head,Tail) end
     ; {Type,Name} when is_atom(Name)
       -> case maps:take(Name,Fill)
            of error -> {head_tail,Rtn,Head,Tail,Fill}
             ; {Took,Rest}
               -> encode_each(Rtn,Nest,{Type,Took},Rest,Head,Tail) end
     ; {comment,Bin} when is_binary(Bin)
       -> Hd = [<<"<!-- ",Bin/binary," -->">>|Head]
        , {head_tail,Rtn,Hd,Tail,Fill}
     ; {declaration,Bin} when is_binary(Bin)
       -> Hd = [<<"<?",Bin/binary," ?>">>|Head]
        , {head_tail,Rtn,Hd,Tail,Fill}
     ; {attribute,Bin} when is_binary(Bin)
       -> Hd = [<<"<!",Bin/binary," !>">>|Head]
        , {head_tail,Rtn,Hd,Tail,Fill}
     ; {'CDATA',Bin} when is_binary(Bin)
       -> Esc = cdata_text(Bin)
        , Hd = [<<"<![CDATA[",Esc/binary,"]]>">>|Head]
        , {head_tail,Rtn,Hd,Tail,Fill}
     ; {nest,Get,Nest_data}
       -> case maps:take(Get,Fill)
            of error -> {head_tail,Rtn,Head,Tail,Fill}
             ; {Got,Rest}
               -> case encode_each(Rtn,[Get|Nest],Nest_data,Got,Head,Tail)
                    of {head_tail,New,Hd,Tl,Empty}
                       when map_size(Empty) =:= 0
                       -> {head_tail,New,Hd,Tl,Rest}
                     ; Other -> Other end end
     ; {node,Tag,Names,Body} when is_binary(Tag), is_map(Names)
       -> {Attr,Rest} = encode_attr(Names,Fill)
        , Empty = [?lt,Tag,Attr,?sol,?gt]
        , Open = [?lt,Tag,Attr,?gt]
        , Close = [?lt,?sol,Tag,?gt]
        , case encode_each(Rtn,Nest,Body,Rest,[Open|Head],[Close])
            of {head_tail,New,[Open|Head],[Close],Rem} when Attr =:= []
               -> {head_tail,New,Head,Tail,Rem}
             ; {head_tail,New,[Open|Head],[Close],Rem}
               -> {head_tail,New,[Empty|Head],Tail,Rem}
             ; {head_tail,New,[<<>>,Open|Head],[Close],Rem}
               -> {head_tail,New,[Empty|Head],Tail,Rem}
             ; {head_tail,New,Hd,Tl,Rem}
               -> Join = [Tl|Hd]
                , {head_tail,New,Join,Tail,Rem}
             ; Other -> Other end end.

encode_attr(Names,Values) ->
  Iter = maps:iterator(Values),
  encode_each_attr(Names,Iter,[],#{}).

encode_each_attr(Names,Iter,Out,Rest) ->
  case maps:next(Iter)
    of none -> {Out,Rest}
     ; {Key,Value,Next}
       -> case Names
            of #{ Key := Name }
               -> Pair = [<<" ">>,Name,<<"=\"">>,Value,<<"\"">>]
                , encode_each_attr(Names,Next,[Pair|Out],Rest)
             ; #{ }
               -> Keep = Rest#{ Key => Value }
                , encode_each_attr(Names,Next,Out,Keep) end end.

body_text(Bin) ->
  body_text_each(0,Bin,<<>>).

body_text_each(N,Bin,Bout) when N < byte_size(Bin) ->
  Conv = #{
      ?lt => <<"&lt;">>,
      ?gt => <<"&gt;">>,
     ?amp => <<"&amp;">>,
    ?apos => <<"&apos;">>,
    ?quot => <<"&quot;">> },
  case Bin
    of << L:N/binary, C, R/binary >>
       -> case Conv
            of #{ C := Esc }
               -> Insert = << Bout/binary, L/binary, Esc/binary >>
                , body_text_each(0,R,Insert)
             ; #{ } -> body_text_each(N + 1,Bin,Bout) end
     ; _ -> body_text_each(N + 1,Bin,Bout) end;
body_text_each(_,Bin,Bout) ->
  << Bout/binary, Bin/binary >>.

cdata_text(Bin) ->
 cdata_text_each(0,Bin,<<>>).

cdata_text_each(N,Bin,Bout) when N < byte_size(Bin) ->
  case Bin
    of << L:N/binary, "]]>", R/binary >>
       -> Insert = << Bout/binary, L/binary, "]]]]><![CDATA[>">>
        , cdata_text_each(0,R,Insert)
     ; _ -> cdata_text_each(N + 1,Bin,Bout) end;
cdata_text_each(_,Bin,Bout) ->
  << Bout/binary, Bin/binary >>.











%%%
%%%   Decode an entire XML message into a nested list of
%%%   terms.  Requires that the message is entire, not a
%%%   fragment.
%%%
decode(Binary) ->
  nested_nodes(
    token_textual(Binary,
      strip_blank(
        token_sequence(Binary,0,byte_size(Binary))))).

%%%
%%%   The token_sequence function scans the Binary to
%%%   locate the start and end positions of token_textual of
%%%   text and of markup elements.  The result is sorted
%%%   linear list of segment positions and what each one
%%%   represents.
%%%
token_sequence(Binary,Base,_) when Base =:= byte_size(Binary) -> [];
token_sequence(Binary,Base,Size) when Base < byte_size(Binary) ->
  case decode_each_segment(Binary,Base,Size)
    of nomatch -> [{nomatch,Base}]
     ; {Type,First,Part,Rest}
       -> Out = {Type,Part}
        , Lead = {binary,{Base,First - Base}}
        , case Size - (Rest - Base)
            of Less when Less > 0
               -> [Lead,Out|token_sequence(Binary,Rest,Less)]
             ; 0 -> [Lead,Out] end end.



-type binary_part() :: {non_neg_integer(),non_neg_integer()}.

-type tag_with_attrib()
      :: binary_part() | binary(). % namespace preserved
-type attrib_name() :: binary().
-type attrib_value() :: binary().
-type document_text()
      :: binary_part() | binary().
-type attribute_pair() :: {attrib_name(),attrib_value()}.


-type doc_contents()
      :: document_text()
       | attribute_pair()
       | { 'attribute' | 'declaration' | 'comment' | 'CDATA'
         , binary_part() }.

-type stack()
      :: [ {tag_with_attrib(),[],[doc_contents()]} ].

-type rule_attrib() :: {atom(),binary()}.
-type rule_name() :: atom() | rule_attrib().
-type rule_pattern() :: [binary()].
-type rules()
      :: [{rule_name(),rule_pattern()}].


-type tag_value()
      :: { atom(), result() | binary() }.
-type element_name()
      :: binary().
-type nest_result()
      :: { 'nested', element_name(), result() }.
-type result()
      :: [ tag_value() | nest_result() ].

-type message_terms() :: [ { atom(), message_terms() | [binary()] } ].
-type tail_fragment() :: binary().



-type context() :: {'continue',stack(),result()}.
-type truncated() :: { 'data', context(), message_terms(), tail_fragment() }.
-type end_of_data() :: { 'data', 'nil', message_terms(), tail_fragment() }.

-spec decode(binary(),context(),ets:tid())
      -> end_of_data()
       | truncated().

decode(Binary,{continue,Stack,Result},Rules) ->
  linear_scan(Binary,0,byte_size(Binary),Stack,Rules,Result);

decode(Binary,_,Rules) when byte_size(Binary) > 0 ->
  linear_scan(Binary,0,byte_size(Binary),[],Rules,[]);
decode(<< >>,_,_) ->
  {data,nil,[],<< >>}.


-define(is_binary_part(X), (is_tuple(X)
 andalso tuple_size(X) =:= 2
 andalso is_integer(element(1,X))
 andalso is_integer(element(2,X)) )).

end_of_input(Binary,Base,Output) ->
  Done = linear_result(Output,[]),
  Tail = binary:part(Binary,{Base,byte_size(Binary) - Base}),
  {data,nil,split_unmatched(Done),Tail}.

linear_result([],Out) -> Out;
linear_result([{Name,Value}|Rest],Out)
when is_atom(Name); is_binary(Name) ->
  linear_result(Rest,[{Name,Value}|Out]);
linear_result([_|Rest],Out) ->
  linear_result(Rest,Out).




truncated(Binary,Base,Stack,Result) ->
  %%
  %%  Result may contain values that have yet to be
  %%  collected into clusters.  These must be stored in
  %%  Retain when linear_scan resumes where it was
  %%  interrupted.
  %%
  %%  The values not Retained, either those in a complete
  %%  cluster, or outside any cluster, are Released and are
  %%  ready for use.
  %%
  {Release,Retain} = trunc_done(Stack,Result,[]),
  Tail = binary:part(Binary,{Base,byte_size(Binary) - Base}),
  Context = {continue,restack(Binary,Stack),Retain}, % where to continue from
  {data,Context,split_unmatched(Release),Tail}.

restack(_,[]) -> [];
restack(Bin,[{Tag,Att,Txt}|Rest]) ->
  Tokens = token_textual(Bin,Txt),
  Pick = omit(Tokens),
  [{Tag,Att,Pick}|restack(Bin,Rest)].

%%%
%%%   The omit/1 function removes tokens that serve no
%%%   purpose when decode resumes, such as comments that do
%%%   not contain document text to extract.  The
%%%   'declaration' and 'attribute' tokens could also be
%%%   omitted, but do not occur as frequently as 'comment'
%%%   that will clutter a long-running decode stack.
%%%
omit([]) -> [];
omit([Comment|Rest]) when element(1,Comment) =:= comment -> omit(Rest);
omit([Each|Rest]) -> [Each|omit(Rest)].

trunc_done(_,[],Release) ->
  {Release,[]};
trunc_done([],Retain,Release) ->
  {Release,Retain};
trunc_done([{Same,[],_}|Stack],[{nested,Same,Retain}],Release) ->
  {Done,Todo} = trunc_done(Stack,Retain,Release),
  {Done,[{nested,Same,Todo}]};
trunc_done([{Same,[_|_],_}|Stack],[{nested,Same,Retain}],Keep) ->
  %%
  %%  An assignment has not been completed.  Values in Keep
  %%  must be retained.
  %%
  {Done,Todo} = trunc_done(Stack,Retain,[]),
  {Done,lists:reverse([{nested,Same,Todo}|Keep])};
trunc_done(Stack,[{Tag,Val}|Rest],Release) when is_binary(Tag), is_binary(Val) ->
  trunc_done(Stack,Rest,[{Tag,Val}|Release]); % binary Tag is unknown element
trunc_done(Stack,[{Tag,Val}|Rest],Release) when is_atom(Tag), is_list(Val) ->
  trunc_done(Stack,Rest,[{Tag,Val}|Release]);
trunc_done(Stack,[{Tag,Val}|Rest],Release) when is_atom(Tag), is_binary(Val) ->
  trunc_done(Stack,Rest,[{Tag,Val}|Release]).

%%%
%%%   The extracted pairs {binary(),binary()} are separated
%%%   from the rest of the {atom(),list()} pairs.  These
%%%   extractions are detected by the configuration
%%%   {{'not_one_of',list()},[binary()]} to identify XML
%%%   elements that contradict the specification.
%%%
%%%   TODO: Values for the 'not_one_of' extraction path
%%%         will be generated from the rest of the defined
%%%         extractions. To be implemented as a function.
%%%
split_unmatched(In) ->
  each_unmatched(In,[],[]).

each_unmatched([],Pass,[]) ->
  lists:reverse(Pass);
each_unmatched([],Pass,Fail) ->
  [{'UNMATCHED',lists:reverse(Fail)}|lists:reverse(Pass)];
each_unmatched([Each|Rest],Pass,Fail) ->
  case Each
    of {Name,Value} when is_atom(Name)
       -> each_unmatched(Rest,[{Name,Value}|Pass],Fail)
     ; {Name,Value} when is_binary(Name)
       -> each_unmatched(Rest,Pass,[{Name,Value}|Fail]) end.
  







-spec linear_scan(
         Binary :: binary(),
         Base   :: non_neg_integer(),
         Size   :: non_neg_integer(),
         Stack  :: stack(),
         Rules  :: rules(),
         Result :: result() )
      -> truncated() | end_of_data().


linear_scan(Binary,Base,Size,Stack,Rules,Result) ->
  case decode_each_segment(Binary,Base,Size)
    of nomatch
       %%
       %%  TODO: Deal with trailing bytes after a complete
       %%        XML message, e.g.  a single trailing
       %%        new-line will cause truncated/4 to be
       %%        called, instead of end_of_input/3
       %%
       -> ?dbg("truncated: nomatch~nStack = ~p.~n",[Stack])
        , truncated(Binary,Base,Stack,Result)
     ; {start_node,First,Part,Rest}
       -> case Stack
            of [] -> Update = Stack % text outside of markup is junk.
             ; [{Top,X,Inside}|Popped]
               -> Lead = push_binary(Base,First,Inside)
                , Update = [{Top,X,Lead}|Popped] end
        , {Elem,Attr} = linear_element(Binary,Part)
        , Here = [E||{E,_,_}<-Stack]
        , Path = strip_namespace([Elem|Here])
        , Select = path_to_rule(Path,Rules)
        , Nest = [{nested,Elem,Result}]
        , Output = extract_attrib(Attr,Select,Nest)
        , Pushed = [{Elem,Select,[]}|Update]
        , case Size - (Rest - Base)
            of Less when Less > 0
               -> linear_scan(Binary,Rest,Less,Pushed,Rules,Output)
             ; 0 -> ?dbg("truncated: start_node~n",[])
                  , truncated(Binary,Rest,Pushed,Output) end
     ; {end_node,First,Part,Rest}
       -> Same = binary:part(Binary,Part)
        , case Stack
            of [{Same,Select,Inside}|Popped]
               -> Lead = push_binary(Base,First,Inside)
                , Value = linear_value(Binary,Lead)
                , Text = text_values(Value,Select,Result)
                , {Same,Nested,Values} = nested(Text,[])
                , case [{Name,Values}||{cluster,Name}<-Select]
                    of [] -> Output = lists:reverse(Values,Nested)
                     ; Cluster -> Output = Cluster ++ Nested end
                , case Size - (Rest - Base)
                    of Less when Less > 0
                       -> linear_scan(Binary,Rest,Less,Popped,Rules,Output)
                     ; 0 when Popped =:= []
                       -> end_of_input(Binary,Rest,Output)
                     ; 0 -> ?dbg("truncated: end_node~n",[])
                          , truncated(Binary,Rest,Popped,Output) end
             ; Fail -> {error,{malformed_input,Fail,Same}} end
     ; {Type,First,Part,Rest}
       -> Out = {Type,Part}
        , case Stack
            of [] -> Pushed = Stack % TODO: xml declaration etc.
             ; [{Top,X,Inside}|Popped]
               -> Lead = [Out|push_binary(Base,First,Inside)]
                , Pushed = [{Top,X,Lead}|Popped] end
        , case Size - (Rest - Base)
            of Less when Less > 0
               -> linear_scan(Binary,Rest,Less,Pushed,Rules,Result)
             ; 0 -> ?dbg("truncated: Type = ~p.~n",[Type])
                  , truncated(Binary,Rest,Pushed,Result) end end.

text_values(_,[],Out) -> Out;
text_values(<<>>,_,Out) -> Out;
text_values(Value,[Name|Rest],Out)
when is_atom(Name); is_binary(Name) ->
  text_values(Value,Rest,[{Name,Value}|Out]);
text_values(Value,[_|Rest],Out) ->
  text_values(Value,Rest,Out).


nested([{nested,Elem,Result}],Out) -> {Elem,Result,Out};
nested([Each|Rest],Out) -> nested(Rest,[Each|Out]).


extract_attrib(_,[],Result) -> Result;
extract_attrib(Attributes,[{attrib,Name,Attr}|Rest],Result)
when is_atom(Name), is_binary(Attr) ->
  Match = strip_namespace(Attr),
  case [V||{A,V}<-Attributes,Match=:=strip_namespace(A)]
    of [] -> extract_attrib(Attributes,Rest,Result)
     ; [Value]
       -> Output = [{Name,Value}|Result]
        , extract_attrib(Attributes,Rest,Output) end;
extract_attrib(Attributes,[_|Rest],Result) ->
  extract_attrib(Attributes,Rest,Result).


push_binary(Same,Same,List) -> List;
push_binary(Base,First,List) when Base < First ->
  [{binary,{Base,First - Base}}|List].


linear_element(Binary,Part) when ?is_binary_part(Part) ->
  Trim = left_white_space(Binary,Part),
  {Left,Right} = split_white_space(Binary,Trim),
  Tag = binary:part(Binary,Left),
  case linear_attrib(Binary,Right,[])
    of nomatch -> nomatch
     ; Attributes -> {Tag,Attributes} end;
linear_element(_,{Tag,Attributes}=Out)
when is_binary(Tag), is_list(Attributes) ->
  Out.

linear_attrib(Bin,Part,Out) ->
  Trim = left_white_space(Bin,Part),
  case binary:match(Bin,<< $= >>,[{scope,Trim}])
    of nomatch -> lists:reverse(Out)
     ; {Equal,1}
       -> {Pos,Len} = Trim
        , Cut = Equal - Pos
        , Name = right_white_space(Bin,{Pos,Cut})
        , case quoted_value(Bin,{Equal + 1,Len - Cut - 1})
            of nomatch -> nomatch
             ; Value
               -> {Vpos,Vlen} = Value
                , Vend = Vpos + Vlen + 1
                , Less = Vend - Pos
                , Attributes = [{binary:part(Bin,Name),escape(Bin,Value)}|Out]
                , linear_attrib(Bin,{Vend,Len - Less},Attributes) end end.

escape(Bin,Part) ->
  Snip = binary:part(Bin,Part),
  Split = binary:split(Snip,<<"&">>,[global]),
  list_to_binary(escape_each(Split)).

escape_each([]) -> [];
escape_each([Snip|Rest]) ->
  case Snip
    of << "quot;", Tail/binary >> -> Head = $"
     ; << "amp;", Tail/binary >> -> Head = $&
     ; << "apos;", Tail/binary >> -> Head = $'
     ; << "lt;", Tail/binary >> -> Head = $<
     ; << "gt;", Tail/binary >> -> Head = $>
     ; _ -> Head = Snip, Tail = << >> end,
  [Head,Tail|escape_each(Rest)].



quoted_value(Bin,Part) ->
  {Pos,Len} = left_white_space(Bin,Part),
  case Bin
    of << _:Pos/binary, Qu, _/binary >>
       when Qu =:= $"; Qu =:= $'
       -> case binary:match(Bin,<< Qu >>,[{scope,{Pos + 1, Len - 1}}])
            of nomatch -> nomatch
             ; {End,1}
               -> Sub = End - Pos - 1
                , {Pos + 1, Sub} end
     ; _ -> nomatch end.

left_white_space(Bin,{Pos,Len}) ->
  case Bin
    of << _:Pos/binary, Ws, _/binary >>
       when Ws =:= $\s; Ws =:= $\t; Ws =:= $\n; Ws =:= $\r
       -> left_white_space(Bin,{Pos + 1, Len - 1})
     ; _ -> {Pos,Len} end.

right_white_space(Bin,{Pos,Len}) ->
  End = Pos + Len,
  case Bin
    of << _:End/binary, Ws, _/binary>>
       when Ws =:= $\s; Ws =:= $\t; Ws =:= $\n; Ws =:= $\r
       -> right_white_space(Bin,{Pos, Len - 1})
     ; _ -> {Pos,Len} end.

split_white_space(Bin,{Pos,Len}) ->
  split_ws_each(Bin,Pos,Pos,Pos + Len).

split_ws_each(_,Pos,End,End) ->
  {{Pos,End - Pos},{End,0}};
split_ws_each(Bin,Pos,Cut,End) when Cut < End ->
  case Bin
    of << _:Cut/binary, Ws, _/binary >>
       when Ws =:= $\s; Ws =:= $\t; Ws =:= $\n; Ws =:= $\r
       -> Len = Cut - Pos
        , {{Pos,Len},left_white_space(Bin,{Cut,End - Cut})}
     ; _ -> split_ws_each(Bin,Pos,Cut + 1,End) end.

strip_namespace([]) -> [];
strip_namespace([Each|Rest]) ->
  [strip_namespace(Each)|strip_namespace(Rest)];
strip_namespace(Binary) when is_binary(Binary) ->
  strip_namespace(Binary,{0,byte_size(Binary)}).

strip_namespace(Binary,{Pos,Len}) ->
  Cut = strip_scoped(Binary,Pos,Pos + Len),
  binary:part(Binary,Cut).

strip_scoped(Binary,Pos,End) ->
  Len = End - Pos,
  Scope = [{scope,{Pos,Len}}],
  case binary:match(Binary,<<$:>>,Scope)
    of nomatch -> {Pos,Len}
     ; {Skip,1}
       -> Cut = Skip + 1
        , strip_scoped(Binary,Cut,End) end.

path_to_rule(_,[]) -> [];
path_to_rule([Head|Same]=Path,[{{not_one_of,List},Same}|Rest]) ->
  case [L||L<-List,L=:=Head]
    of [_] -> path_to_rule(Path,Rest)
     ; [] -> [Head|path_to_rule(Path,Rest)] end;
path_to_rule(Path,[{Name,Path}|Rest]) ->
  [Name|path_to_rule(Path,Rest)];
path_to_rule(Path,[_|Rest]) ->
  path_to_rule(Path,Rest).

linear_value(Binary,Parts) ->
  linear_value_each(Binary,Parts,[]).

linear_value_each(_,[],Out) ->
  list_to_binary(Out);
linear_value_each(Binary,[{binary,Part}|Rest],Out) ->
  Esc = escape(Binary,Part),
  linear_value_each(Binary,Rest,[Esc|Out]);
linear_value_each(Binary,[{'CDATA',Part}|Rest],Out) ->
  Cdata = binary:part(Binary,Part),
  linear_value_each(Binary,Rest,[Cdata|Out]);
linear_value_each(Binary,[_|Rest],Out) ->
  linear_value_each(Binary,Rest,Out).

%%%
%%%   The decode_each_segment function performs a linear
%%%   scan of the Binary, seeking markup by matching the
%%%   '<' and '>' brackets that separate these from the
%%%   document text.
%%%
-type node_type()
      :: empty_node
       | start_node
       | end_node
       | attribute
       | declaration
       | comment
       | 'CDATA'.
-type tag_pos_len()
      :: {Pos::non_neg_integer(),Len::non_neg_integer()}.
-type node_head_pos()
      :: non_neg_integer().
-type node_tail_pos()
      :: non_neg_integer().
-spec decode_each_segment(Binary::binary(),Base::integer(),Size::integer())
      -> nomatch
       | {node_type(),node_head_pos(),tag_pos_len(),node_tail_pos()}.
decode_each_segment(Binary,Base,Size) ->
  Scope = [{scope,{Base,Size}}],
  %%
  %%  https://www.w3.org/TR/xml/#syntax
  %%      The right angle bracket (>) *may* be represented
  %%      using the string &gt;
  %%  The use of "may" implies that > can legally appear in
  %%  XML document text instead of &gt;
  %%
  Heads =
   #{ << ">"         >> => ignore % ideally should be escaped as &gt;
    , << "<"         >> => #{ << ?quot >> => quote
                            , << ?apos >> => quote
                            , <<  "/>" >> => empty_node
                            , <<   ">" >> => start_node}
    , << "</"        >> => #{ <<   ">" >> => end_node }
    , << "<!"        >> => #{ <<  "!>" >> => attribute }
    , << "<?"        >> => #{ <<  "?>" >> => declaration }
    , << "<!--"      >> => #{ << "-->" >> => comment }
    , << "<![CDATA[" >> => #{ << "]]>" >> => 'CDATA'} },
  segment_start(Binary,Heads,Scope).

segment_start(Binary,Heads,Scope) ->
  case match(Binary,Heads,Scope)
    of nomatch -> nomatch
     ; {{Pos,Len},ignore}
       -> Rescope = rescope(Pos + Len,Scope)
        , if Rescope =:= error -> nomatch
           ; true -> segment_start(Binary,Heads,Rescope) end
     ; {{Pos,_},Tails}
       -> case match(Binary,Tails,Scope)
            of nomatch -> nomatch
             ; {{Tpos,Tlen},Tag}
               -> Len = Tpos - Pos + Tlen
                , Part = tag_pos_len(Pos,Len,Tag)
                , {Tag,Pos,Part,Pos + Len} end end.

match(Binary,Map,Scope) when is_binary(Binary), is_map(Map) ->
  case binary:match(Binary,maps:keys(Map),Scope)
    of nomatch -> nomatch
     ; Part
       -> Key = binary:part(Binary,Part)
        , case Map
            of #{ Key := quote }
                -> {Lquote,1} = Part
                 , Rescope = rescope(Lquote + 1,Scope)
                 , if Rescope =:= error -> Rquote = nomatch
                    ; true
                      -> case binary:match(Binary,Key,Rescope)
                           of {Rquote,1} -> true
                            ; nomatch -> Rquote = nomatch end end
                 , if Rquote =:= nomatch -> nomatch
                    ; true
                      -> Restart = rescope(Rquote + 1,Scope)
                       , if Restart =:= error -> nomatch
                          ; true -> match(Binary,Map,Restart) end end
             ; #{ Key := Out } -> {Part,Out}
             ; #{ } -> nomatch end end.

rescope(Rebase,[{scope,{Base,Size}}]) ->
  Resize = Size - (Rebase - Base),
  if Resize > 0 -> [{scope,{Rebase,Resize}}]
   ; true -> error end.

tag_pos_len(Pos,Len,Tag) ->
  case Tag
    of empty_node -> {1 + Pos,Len - 3}
     ; start_node -> {1 + Pos,Len - 2}
     ;   end_node -> {2 + Pos,Len - 3}
     ;  attribute -> {2 + Pos,Len - 4}
     ;declaration -> {2 + Pos,Len - 4}
     ;    comment -> {4 + Pos,Len - 7}
     ;    'CDATA' -> {9 + Pos,Len - 12}
     ;       text -> {Pos,Len} end.


%%%
%%%   The token_textual function receives the serial
%%%   sequence of interleaved text and markup positions
%%%   from the token_sequence function, and extracts the
%%%   binary fragments that they represent.
%%%
token_textual(_,[]) -> [];
token_textual(Binary,[Each|Rest]) ->
  case token_text_each(Binary,Each)
    of {nomatch,Pos} -> [ {nomatch,Pos} ]
     ; Node -> [ Node | token_textual(Binary,Rest) ] end.


token_text_each(Binary,{Type,Each})
when Type =:= empty_node
   ; Type =:= start_node
   ; Type =:= end_node
   ; Type =:= attribute
   ; Type =:= declaration
   ; Type =:= comment
   ; Type =:= 'CDATA' ->
  case Each
    of Done when is_binary(Done) -> {Type,Done}
     ; Part -> {Type,binary:part(Binary,Part)} end;
token_text_each(Binary,Each) ->
  case Each
    of Done when is_binary(Done) -> Done
     ; {nomatch,Pos}
       -> {nomatch,Pos}
     ; {binary,Part}
       -> binary:part(Binary,Part) end.


%%%
%%%   The strip_blank function removes redundant data that
%%%   is does not sit immediately between a start and an
%%%   end markup element.
%%%
%%%   BUG: too brutal, ignores 'CDATA'...
%%%
strip_blank([]) -> [];
strip_blank([{nomatch,_}]) -> [];
strip_blank([{start_node,_}=L,{binary,_}=Bin,{end_node,_}=R|Rest]) ->
  [L,Bin,R|strip_blank(Rest)];
strip_blank([{binary,_}|Rest]) ->
  strip_blank(Rest);
strip_blank([Each|Rest]) ->
  [Each|strip_blank(Rest)].

%%%
%%%   The nested_nodes function takes the serial sequence
%%%   of text and markup token_textual, and matches the
%%%   opening and closing markup elements to produce a
%%%   nested list representation that reflects the document
%%%   structure.
%%%
nested_nodes(In) ->
  nested_nodes(In,[]).

nested_nodes([],Out) -> lists:reverse(Out);
nested_nodes([{end_node,End}|In],Out) -> {end_node,End,lists:reverse(Out),In};
nested_nodes([{start_node,Start}|In],Out) ->
  ?pdict_trace(Start),
  case nested_nodes(In)
    of {end_node,Tag,[<<>>],Rest}
       -> Content = nested_node_contents(Start,[],Tag)
        , Node = {node,Tag,[{when_empty,start_and_end}],Content}
     ; {end_node,Tag,[_|_]=Nest,Rest}
       -> Content = nested_node_contents(Start,Nest,Tag)
        , Node = {node,Tag,[],Content} end,
  nested_nodes(Rest,[Node|Out]);
nested_nodes([{empty_node,Empty}|In],Out) ->
  White = [<<$\s>>,<<$\t>>,<<$\n>>,<<$\r>>],
  case binary:split(Empty,White,[trim_all])
    of [Tag]
       -> Node = {node,Tag,[{when_empty,keep_element}],[]}
     ; [Tag,X]
       -> Attributes = decode_attrib(X,[])
        , Node = {node,Tag,[{when_empty,keep_element}],Attributes} end,
  nested_nodes(In,[Node|Out]);
nested_nodes([Each|In],Out) ->
  nested_nodes(In,[Each|Out]).

nested_node_contents(Start,Content,Tag) ->
  ?pdict_trace(Tag),
  Size = byte_size(Tag),
  case Start
    of Tag -> Content
     ; << Tag:Size/binary, X/binary >>
       -> decode_attrib(X,Content) end.

decode_attrib(Bin,Out) ->
  Equal = [<<$=>>],
  case binary:split(Bin,Equal,[global,trim_all])
    of [Bin] -> Out
     ; Split
       -> Pairs = decode_each_attrib(Split,[])
        , lists:reverse(Pairs,Out) end.

decode_each_attrib([_],Out) -> Out;
decode_each_attrib([Left,Right|In],Out) ->
  White = [<<$\s>>,<<$\t>>,<<$\n>>,<<$\r>>],
  [Name] = binary:split(Left,White,[global,trim_all]),
  Delim = [<<$">>,<<$'>>],
  case binary:split(Right,Delim,[global])
    of [<< >>,Value,<< >>] -> [{Name,Value}|Out]
     ; [<< >>,Value,Rest]
       -> decode_each_attrib([Rest|In],[{Name,Value}|Out]) end.

load_config(Name) -> plexus:load_config(Name).
