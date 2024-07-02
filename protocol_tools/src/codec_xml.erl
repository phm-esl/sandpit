-module(codec_xml).

-export(
 [ encode/1
 , encode/2
 , decode/1
 , decode_hook/1 ]).

-define(space,$\s).
-define(tab,$\t).
-define(return,$\r).
-define(linefeed,$\n).

-define(hyphen,$-).
-define(less_than,$<).
-define(equal,$=).
-define(greater_than,$>).
-define(ampersand,$&).
-define(percent,$%).
-define(semicolon,$;).
-define(slash,$/).

-define(empty_element,"/>").
-define(end_tag,"</").

-define(CDATA_start,"<![CDATA[").
-define(CDATA_end,"]]>").

-define(comment_start,"<!--").
-define(comment_end,"-->").

-define(processsing_instruction_start,"<?").
-define(processsing_instruction_end,"?>").

-define(declaration_start,"<!").
-define(declaration_end,">").

%%%
%%%   TODO: Either use a map instead of record, or move
%%%         definition to header file.
%%%
-record(element,
  { name = << >>,
    attributes = #{},
    content = [] }).

%-define(log(F,A),logger:notice("~p:~p:~p~n\t"++F,[?MODULE,?FUNCTION_NAME,?LINE|A])).
-define(log(F,A),io:format("~p:~p:~p~n\t"++F,[?MODULE,?FUNCTION_NAME,?LINE|A])).



encode(In) ->
  Done = fun (Out) -> erlang:list_to_binary(Out) end,
  encode_only(Done,In).

encode(In,Map) ->
  Done = fun (Out) -> to_binary(Out) end,
  Trail = {Map,[]},
  encode_inject(Done,Trail,In).


encode_inject(Done,Trail,#element{} = Element) ->

  Update = fun ({Inject,Attr}) ->
    % Here the caller gave content to Inject, and Attr to change too
    Next = fun (Encoded) ->
      Merged = maps:merge(Element#element.attributes,Attr),
      encode_element(Done,
        Element#element{ attributes = Merged, content = Encoded }) end,
    encode_inject(Next,Trail,Inject) end,

  case to_atom(trim(Element#element.name)) of
    [] ->
      % no modifications to this Element nor its contents
      encode_only(Done,Element);
    [Atom] ->
      {Action,Where} = Trail,
      Here = [Atom|Where],
      Content = Element#element.content,
      case Action of
        #{ Atom := Into } when is_map(Into) ->
          if Content == []; Content =:= empty -> encode_only(Done,Element);
             true ->
               % Enter into the Content that can eventually be modified
               Next = fun (Encoded) ->
                 encode_element(Done, Element#element{ content = Encoded } ) end,
               encode_inject(Next,{Into,Here},Content) end;
        #{ {xpath,Atom} := #{ attr := Attr }, Atom := Inject } ->
          % return control to caller to decide how to treat this element
          % the Action says to also deal with Attr changes too
          Original = {Content,Element#element.attributes},
          {Update,Original,[{Atom,Inject,Attr}|Where]};
        #{ Atom := Inject } ->
          % return control to caller to decide how to treat this element
          Attr = #{},
          Original = {Content,Element#element.attributes},
          {Update,Original,[{Atom,Inject,Attr}|Where]};
        #{ } ->
          % The Name is not located here in the Action, no modifications.
          encode_only(Done,Element) end end;
encode_inject(Done,Trail,Content) when is_list(Content) ->
  encode_list_inject(Done,Trail,Content);
encode_inject(Done,_,Last) ->
  encode_last(Done,Last).

encode_list_inject(Done,Trail,In) ->
  encode_list_inject(Done,Trail,In,[]).

encode_list_inject(Done,_,[],Out) ->
  Done(lists:reverse(Out));
encode_list_inject(Done,Trail,[Each|Rest],Out) ->
  Back = fun (Fill) -> encode_list_inject(Done,Trail,Rest,[Fill|Out]) end,
  encode_inject(Back,Trail,Each).




encode_only(Done,#element{} = Element) ->
  % first encode the Content, Next wrap the Encoded result with the tags
  % and attributes.
  Next = fun (Encoded) ->
    encode_element(Done, Element#element{ content = Encoded } ) end,
  encode_only(Next,Element#element.content);
encode_only(Done,Content) when is_list(Content) ->
  encode_list_only(Done,Content);
encode_only(Done,Last) ->
  encode_last(Done,Last).

encode_list_only(Done,In) -> encode_list_only(Done,In,[]).

encode_list_only(Done,[],Out) ->
  Done(lists:reverse(Out));
encode_list_only(Done,[Each|Rest],Out) ->
  Back = fun (Fill) -> encode_list_only(Done,Rest,[Fill|Out]) end,
  encode_only(Back,Each).


encode_last(Done,Fill) when is_binary(Fill) ->
  Done(Fill);
encode_last(Done,empty) ->
  Done(<<>>);
encode_last(Done,{entity_ref,Ref}) ->
  Done(<< $&, Ref/binary, $; >>);
encode_last(Done,{prolog,Prolog}) ->
  Done(<< "<?xml", Prolog/binary, "?>" >>);
encode_last(Done,{'CDATA',Cdata}) ->
  Done(<< "<![CDATA[", Cdata/binary, "]]>" >>);
encode_last(Done,Slot) when is_map(Slot) ->
  % Postpone encode, leave this Slot as is for value insertion later.
  Done(Slot).


encode_element(Done,#element{ } = In) ->
  %
  % Value of Content must be an io_list, no tuples, no maps etc.
  %
  % This requires that encode_element/1 is ONLY called from within the
  % function closure that is passed as first parameter to either
  % encode_inject/3 or encode_only/2, such that it is called only when the
  % closing tag of an XML element is encountered, and thus the Contents of
  % the In element are encoded for certain.
  %
  #element{ name = Name, attributes = Attributes, content =  Content } = In,
  Attr = encode_attributes(Attributes),
  case Content of
    Empty when Empty =:= empty; Empty =:= [] ->
      Done([ ?less_than, Name, Attr, ?empty_element ]);
    _ ->
      Done(
        [ ?less_than, Name, Attr, ?greater_than,
          Content,
          ?end_tag, Name, ?greater_than ] ) end.






to_binary(Nbr) when is_integer(Nbr) -> erlang:integer_to_binary(Nbr);
to_binary(Bin) when is_binary(Bin) -> Bin;
to_binary(List) ->
  Done = fun (I) -> I end,
  to_binary(Done,lists:flatten(List),[]).


to_binary(Done,Rest,[H|T]) when not is_binary(H), not is_integer(H) ->
  Next = fun(In) ->
    Tail = case In of
      <<>> -> [];
      Bin when is_binary(Bin) -> [Bin];
      List when is_list(List) -> List end,
    case erlang:list_to_binary(lists:reverse(T)) of
      <<>> -> Done([H|Tail]);
      Head -> Done([Head,H|Tail]) end end,
  to_binary(Next,Rest,[]);
to_binary(Done,[H|T],Out) ->
  to_binary(Done,T,[H|Out]);
to_binary(Done,[],Out) ->
  Done(erlang:list_to_binary(lists:reverse(Out))).








trim(Bin) when is_binary(Bin) ->
  lists:last(binary:split(Bin,<<$:>>,[global])).

to_atom(Bin) when is_binary(Bin) ->
  %%
  %%  TODO: avoid using the atom table...
  %%
  try erlang:binary_to_existing_atom(Bin) of Atom when is_atom(Atom) -> [Atom]
  catch error:badarg -> [] end.



encode_attributes(Attr) when is_map(Attr) ->
  maps:fold(fun encode_each_attr/3,[],Attr).

encode_each_attr(Name,Value,Out) ->
  Quote = quote_value(Value),
  [ ?space, Name, ?equal, Quote, Value, Quote | Out ].

quote_value(Value) -> quote_value(0,Value).

quote_value(Pos,Bin) when Pos < size(Bin) ->
  case Bin of
    << _:Pos/binary, $", _/binary >> -> $';
    << _:Pos/binary, $', _/binary >> -> $";
    _ -> quote_value(Pos + 1,Bin) end;
quote_value(_,_) -> $".


%%%
%%%   Parser that hews closely to the grammer as specified in
%%%   https://www.w3.org/TR/xml/
%%%   All XML documents begin with the prolog. No leading spaces.
%%%
decode(Binary) ->
  loop(decode_hook(Binary)).

loop({_,Fn}) when is_function(Fn) -> loop(Fn());
loop(Decoded) -> Decoded.

decode_hook(<< ?processsing_instruction_start, Tail/binary >>) ->
  process_instruct(Tail,fun prolog/2).

prolog(Bin,{process_instruct,<< "xml", PI/binary>>}) ->
  Next = fun(T) -> doctypedecl(T,{prolog,PI}) end,
  white_space(Bin,Next).

%%%
%%%   Either one or none of doctypedecl, followed by misc.
%%%
doctypedecl(Bin,Prolog) ->
  Done = fun (I,O) -> doctypedecl_end(I,O) end,
  case Bin of
    << "<!DOCTYPE", Tail/binary >> ->
      Next = fun (T,{declaration,_}=Decl) ->
        misc_space(T,[Decl,Prolog],Done) end,
      declaration(Tail,Next);
    _ -> misc_space(Bin,[Prolog],Done) end.

doctypedecl_end(Bin,Out) ->
  Done = fun (<< >>,Result) -> lists:reverse(Result) end,
  Misc = fun (T,Element) -> misc_space(T,[Element|Out],Done) end,
  case Bin of
    << >> -> Out;
    << ?less_than, Tail/binary >> when Tail /= << >> ->
      Next = fun(T) -> start_tag_name(T,Misc) end,
      white_space(Tail,Next);
    _ ->
      fun (More) when is_binary(More) ->
        doctypedecl_end(<< Bin/binary, More/binary >>,Out) end end.

%%%
%%%   Only exists at the document top level.
%%%
misc_space(Bin,Out,Done) ->
  white_space(Bin,fun (T) -> misc(T,Out,Done) end).

misc(Bin,Out,Done) ->
  Again = fun(I,O) -> misc_space(I,[O|Out],Done) end,
  case Bin of
    << ?processsing_instruction_start, Tail/binary >> ->
      process_instruct(Tail,Again);
    << ?comment_start, Tail/binary >> ->
      comment(Tail,Again);
    _ -> Done(Bin,Out) end.


cdata_section(In,Done) -> cdata_section(In,0,Done).

cdata_section(In,Pos,Done) ->
  case In of
    << >> ->
      fun (More) when is_binary(More) ->
        cdata_section(<< In/binary, More/binary >>,Pos,Done) end;
    << CDATA:Pos/binary, ?CDATA_end, Tail/binary >> ->
      Out = {'CDATA',CDATA},
      result(Tail,Out,Done);
    _ -> cdata_section(In,Pos + 1,Done) end.


comment(In,Done) when is_function(Done)  -> comment(In,0,Done).

comment(In,Pos,Done) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) when is_binary(More) ->
        comment(<< In/binary, More/binary >>,Pos,Done) end;
    << Comment:Pos/binary, ?comment_end, Tail/binary >> ->
      Out = {comment,Comment},
      Done(Tail,Out);
    << _:Pos/binary, ?hyphen,B, _/binary >> when B /= ?hyphen ->
      comment(In,Pos + 1,Done); % double-hyphen MUST NOT occur within comments
    << _:Pos/binary, A, _/binary >> when A /= $- ->
      comment(In,Pos + 1,Done) end.


process_instruct(In,Done) when is_function(Done) ->
  process_instruct(In,0,Done).

process_instruct(In,Pos,Done) ->
  Resume = fun (More) when is_binary(More) ->
        process_instruct(<< In/binary, More/binary >>,Pos,Done) end,
  case In of
    << _:Pos/binary >> -> Resume;
    << _:Pos/binary, $? >> -> Resume;
    << Prolog:Pos/binary, ?processsing_instruction_end, Tail/binary >> ->
      Out = {process_instruct,Prolog},
      Done(Tail,Out);
    _ -> process_instruct(In,Pos + 1,Done) end.


%%%
%%%   doctypedecl
%%%   ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
%%%
%%%   ExternalID
%%%   ::= 'SYSTEM' S SystemLiteral
%%%     | 'PUBLIC' S PubidLiteral S SystemLiteral
%%%
%%%   SystemLiteral
%%%   ::= ('"' [^"]* '"')
%%%     | ("'" [^']* "'")
%%%
%%%   PubidLiteral
%%%   ::= '"' PubidChar* '"'
%%%     | "'" (PubidChar - "'")* "'"
%%%   PubidChar
%%%   ::= #x20
%%%     | #xD
%%%     | #xA
%%%     | [a-zA-Z0-9]
%%%     | [-'()+,./:=?;!*#@$_%]
%%%
%%%   intSubset   ::=   (markupdecl | DeclSep)*
%%%
%%%   markupdecl
%%%   ::=   elementdecl
%%%     | AttlistDecl
%%%     | EntityDecl
%%%     | NotationDecl
%%%     | PI
%%%     | Comment
%%%
%%%   DeclSep   ::=   PEReference | S
%%%
%%%   PEReference   ::=   '%' Name ';'
%%%
declaration(In,Done) -> declaration(In,0,Done).

declaration(In,Pos,Done) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) when is_binary(More) ->
        declaration(<< In/binary, More/binary >>,Pos,Done) end;
    << Declaration:Pos/binary, ?declaration_end, Tail/binary >> ->
      result(Tail,{declaration,Declaration},Done);
    _ -> declaration(In,Pos + 1,Done) end.


entity_reference(In,Done) ->
  reference(In,0,entity_ref,Done).

%%%   PEReference   ::=   '%' Name ';'
%param_entity_reference(In,Done) ->
%  reference(In,0,param_ref,Done).

reference(In,Pos,Type,Done) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) when is_binary(More) ->
        reference(<< In/binary, More/binary >>,Pos,Type,Done) end;
    << Reference:Pos/binary, ?semicolon, Tail/binary >> ->
      result(Tail,{Type,Reference},Done);
    _ -> reference(In,Pos + 1,Type,Done) end.


start_tag_name(In,Done) when is_function(Done) ->
  Next = fun (Tail,Elem_name) -> attr_name(Tail,Elem_name,#{},Done) end,
  Hook = fun (I,O) -> hook(I,O,Next) end,
  token(In,0,Hook).

attr_name(In,Elem_name,Attributes,Done) ->
  Next = fun (Tail,Attr_name) ->
    Pair = fun (Exit,Attr_value) ->
      Out = Attributes#{ Attr_name => Attr_value },
      attr_name(Exit,Elem_name,Out,Done) end,
    attr_value(Tail,Pair) end,
  case In of
    << ?empty_element, Tail/binary >> ->
      Out = #element{
        name = Elem_name,
        attributes = Attributes,
        content = empty },
      hook(Tail,Out,Done);
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
      white_space(Tail, fun (T) -> result(T,Out,Done) end);
    _ -> quote(In,Pos + 1,Quote,Done) end.



%%%   content
%%%   ::= CharData? ( ( element
%%%                   | Reference
%%%                   | CDSect
%%%                   | PI
%%%                   | Comment ) CharData? )*
content(In,Pos,Out,Done) ->
  Content = Out#element.content,
  case In of
    << _:Pos/binary >> -> fun (More) ->
      content(<< In/binary, More/binary >>,Pos,Out,Done) end;
    << Text:Pos/binary, Tail/binary >> ->
      Next = fun(T,Elem) ->
        Update = if
          Pos > 0 -> Out#element{ content = [ Elem,Text | Content ] };
          Pos == 0 -> Out#element{ content = [ Elem | Content ] } end,
        content(T,0,Update,Done) end,
      case Tail of
        << ?end_tag, Rest/binary >> ->
          Update = if
            Pos > 0 -> Out#element{ content = [ Text | Content ] };
            Pos == 0 -> Out end,
          end_tag(Rest,Update,Done);
        << ?ampersand, Rest/binary >> ->
          entity_reference(Rest,Next);
        << ?CDATA_start, Rest/binary >> ->
          cdata_section(Rest,Next);
        << ?comment_start, Rest/binary >> ->
          comment(Rest,Next);
        << ?processsing_instruction_start, Rest/binary >> ->
          process_instruct(Rest,Next);
        << ?less_than, Rest/binary >> when Rest /= << >> ->
          Element = fun(T) -> start_tag_name(T,Next) end,
          white_space(Rest,Element);
        _ -> content(In,Pos + 1,Out,Done) end end.


end_tag(In,Out,Done) ->
  Elem_name = Out#element.name,
  Pos = byte_size(Elem_name),
  Match = fun Resume(Tag) ->
    case Tag of
      << Elem_name:Pos/binary, Tail/binary >> ->
        Content = Out#element.content,
        Update = Out#element{ content = lists:reverse(Content) },
        Next = fun
          Fn(<< ?greater_than, T/binary >>) -> hook(T,Update,Done);
          Fn(<< >>) -> fun (More) -> Fn(More) end end,
        white_space(Tail,Next);
      _ when byte_size(Tag) < Pos ->
        fun (More) -> Resume(<< Tag/binary, More/binary >>) end end end,
  white_space(In,Match).


token(In,Pos,Done) ->
  case In of
    << _:Pos/binary >> ->
      fun (More) -> token(<< In/binary, More/binary >>,Pos,Done) end;
    << _:Pos/binary, Ch, _/binary >>
    when Ch /= ?slash,
         Ch /= ?greater_than,
         Ch /= ?equal,
         Ch /= ?linefeed,
         Ch /= ?return,
         Ch /= ?space,
         Ch /= ?tab -> token(In,Pos + 1,Done);
    << Token:Pos/binary, Tail/binary >> when Pos > 0 ->
      white_space(Tail, fun (T) -> result(T,Token,Done) end) end.

white_space(In,Done) ->
  case In of
    << Sp, Tail/binary >>
    when Sp =:= ?linefeed;
         Sp =:= ?return;
         Sp =:= ?space;
         Sp =:= ?tab -> white_space(Tail,Done);
    _ -> Done(In) end.


%%%
%%%   API hook: temporarily pass control back to the
%%%   calling function to offer the element Contents for
%%%   examination, and a continuation.  When examination
%%%   concludes, decoding continues by calling the
%%%   continuation function closure.
%%%
hook(In,Out,Done) when is_function(Done) ->
  Next = fun () -> Done(In,Out) end,
  {Out,Next}.

result(In,Out,Done) when is_function(Done) -> Done(In,Out).
