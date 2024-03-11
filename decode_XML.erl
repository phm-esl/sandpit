-module(decode_XML).

-compile([export_all]).

-export([
  decode/1,
  test/0 ]).

-define(space,$\s).
-define(tab,$\t).
-define(return,$\r).
-define(linefeed,$\n).

-define(hyphen,$-).
-define(less_than,$<).
-define(equal,$=).
-define(greater_than,$>).

-define(CDATA_start,"<![CDATA[").
-define(CDATA_end,"]]>").

-define(comment_start,"<!--").
-define(comment_end,"-->").

-record(element,
  { name = << >>,
    attributes = #{},
    content = [] }).

decode(In) when is_binary(In) ->
  Start = fun (Tail) -> decode(Tail,fun done/2) end,
  white_space(In,Start).


decode(In,Done) ->
  case In of
    << >> ->
      fun (More) when is_binary(More) ->
        decode(More,Done) end;
%    << ?greter_than, _/binary >> ->
%      decode(In,Pos + 1,Done);
    << ?CDATA_start, Tail/binary >> ->
      cdata_section(Tail,0,Done);
    << ?comment_start, Tail/binary >> ->
      comment(Tail,0,Done);
    << ?less_than, Tail/binary >> ->
      Next = fun(T) -> start_tag_name(T,Done) end,
      white_space(Tail,Next) end.



cdata_section(In,Pos,Done) ->
  case In of
    << >> ->
      fun (More) when is_binary(More) ->
        cdata_section(<< In/binary, More/binary >>,Pos,Done) end;
    << CDATA:Pos/binary, ?CDATA_end, Tail/binary >> ->
      Done(Tail,{'CDATA',CDATA});
    _ -> cdata_section(In,Pos + 1,Done) end.

comment(In,Pos,Done) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) when is_binary(More) ->
        comment(<< In/binary, More/binary >>,Pos,Done) end;
    << Comment:Pos/binary, ?comment_end, Tail/binary >> ->
      Done(Tail,{comment,Comment});
    << _:Pos/binary, ?hyphen,B, _/binary >> when B /= ?hyphen ->
      comment(In,Pos + 1,Done); % double-hyphen MUST NOT occur within comments
    << _:Pos/binary, A, _/binary >> when A /= $- ->
      comment(In,Pos + 1,Done) end.








start_tag_name(In,Done) ->
  Next = fun (Tail,Elem_name) -> attr_name(Tail,Elem_name,#{},Done) end,
  token(In,0,Next).

attr_name(In,Elem_name,Attributes,Done) ->
  Next = fun (Tail,Attr_name) ->
    Pair = fun (Exit,Attr_value) ->
      Out = Attributes#{ Attr_name => Attr_value },
      attr_name(Exit,Elem_name,Out,Done) end,
    attr_value(Tail,Pair) end,
  case In of
    << "/>", Tail/binary >> ->
      Out = #element{
        name = Elem_name,
        attributes = Attributes,
        content = empty },
      Done(Tail,Out);
    << ?greater_than, Tail/binary >> ->
      Out = #element{
        name = Elem_name,
        attributes = Attributes },
      content(Tail,0,Out,Done);
    _ -> token(In,0,Next) end.

attr_value(In,Done) ->
  case In of
    << $=, $", Tail/binary >> -> quote(Tail,0,$",Done);
    << $=, $', Tail/binary >> -> quote(Tail,0,$',Done) end.

quote(In,Pos,Quote,Done) ->
  % Not parsing Entity Replacement Text, e.g. &...; and %...; are untouched.
  case In of
    << Out:Pos/binary, Q, Tail/binary >> when Q =:= Quote ->
      white_space(Tail, fun (T) -> Done(T,Out) end);
    _ -> quote(In,Pos + 1,Quote,Done) end.







content(In,Pos,Out,Done) ->
  Content = Out#element.content,
  case In of
    << Text:Pos/binary, "</", Tail/binary >> ->
      Update = if
        Pos > 0 -> Out#element{ content = [ Text | Content ] };
        Pos == 0 -> Out end,
      end_tag(Tail,Update,Done);
    << Text:Pos/binary, Tail/binary >> ->
      case binary:first(Tail) of
        ?less_than ->
          Next = fun (T,Element) ->
            Update = if
              Pos > 0 -> Out#element{ content = [ Element, Text | Content ] };
              Pos == 0 -> Out#element{ content = [ Element | Content ] } end,
            content(T,0,Update,Done) end,
          decode(Tail,Next);
        _ -> content(In,Pos + 1,Out,Done) end end.

end_tag(In,Out,Done) ->
  Elem_name = Out#element.name,
  Pos = byte_size(Elem_name),
  Match = fun (Tag) ->
    case Tag of
      << Elem_name:Pos/binary, Tail/binary >> ->
        Content = Out#element.content,
        Update = Out#element{ content = lists:reverse(Content) },
        Next = fun (<< ?greater_than, T/binary >>) -> Done(T,Update) end,
        white_space(Tail,Next) end end,
  white_space(In,Match).






token(In,Pos,Done) ->
  case In of
    << _:Pos/binary, Ch, _/binary >>
    when Ch /= ?greater_than,
         Ch /= ?equal,
         Ch /= ?linefeed,
         Ch /= ?return,
         Ch /= ?space,
         Ch /= ?tab -> token(In,Pos + 1,Done);
    << Token:Pos/binary, Tail/binary >> when Pos > 0 ->
      white_space(Tail, fun (T) -> Done(T,Token) end) end.

white_space(In,Done) ->
  case In of
    << Sp, Tail/binary >>
    when Sp =:= ?linefeed;
         Sp =:= ?return;
         Sp =:= ?space;
         Sp =:= ?tab -> white_space(Tail,Done);
    _ -> Done(In) end.


next(In,Out) ->
  case In of
    << >> ->
      fun (More) when is_binary(More) -> next(More,Out) end;
    << "</", _/binary >> ->
      Out;
    _ -> white_space(In,fun(T) -> [Out|decode(T,fun next/2)] end) end.

done(In,Out) ->
  Next = fun (T) -> [Out|decode(T,fun done/2)] end,
  white_space(In,Next).


test() ->
  In =  << "

  <!-- Some sort of-
  commentary... --> <![CDATA[<![CDATA[]]]]><![CDATA[>]]> < element abc=\"123\" def='987' >
 something... < inside /><deeper>bottom</deeper></ element >
< another empty='true' />

" >>,

  Out = decode(In),
  case Out of
    [ {comment,<< " Some sort of-
  commentary... " >>},
      {'CDATA',<<"<![CDATA[]]">>},
      {'CDATA',<<">">>},
      {element,<< "element" >>,#{},
       [ << "\n something... " >>,
         {element,<<"inside">>,#{},empty},
         {element,<<"deeper">>,#{},[<<"bottom">>]} ]},
      {element,<< "another" >>,#{},empty}
    | Fn ] when is_function(Fn,1) -> Out end.
