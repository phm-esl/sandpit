

# `codec_JSON` examples

The JSON encoder offers the option to substitute atoms contained in the
input data with arbitrary values. The atoms can optionally be encoded as
string values if the encoder is called with the option switch turned on:

    1> Out = codec_JSON:encode(
      {1,c,{3,4,#{"a" => a, "b" => b}}},
      #{atomic_names => true}).
    <<"[1,\"c\",[3,4,{\"b\":\"b\",\"a\":\"a\"}]]">>
    2> io:put_chars([Out,$\n]).
    [1,"c",[3,4,{"b":"b","a":"a"}]]
    ok

The default is to handle atoms by requesting substitutions as these are
encountered while processing the input data.

Kick off the encoding process by offering data that contains `atom()`
symbols to represent place-holders:

    1> {c,Fn0} = codec_JSON:encode({1,c,{3,4,#{"a" => a, "b" => b}}}).
    {c,#Fun<codec_JSON.1.26682825>}

The encoder returns a two-tuple, the first element is the atom `'c'`, this
serves as a key to identify what value to insert at that position.  Here we
choose to insert an array `{11,22,33}` (Erlang tuples are encoded as JSON
arrays).

The returned function in `1>` is called with the `{11,22,33}` substitution
for `'c'` in the encoded output.  The next place-holder returned by the
encoder is the atom `'b'`:

    2> {b,Fn1} = Fn0({11,22,33}).
    {b,#Fun<codec_JSON.1.26682825>}

For `'b'` the object `#{"cc" => 44, "dd" => 55}` is substituted (Erlang maps
are encoded as JSON objects):

    3> {a,Fn2} = Fn1(#{"cc" => 44, "dd" => 55}).
    {a,#Fun<codec_JSON.1.26682825>}

The last place-holder is atom `'a'`, which is substituted with a string
value `"END"`:

    4> Bin = Fn2("END").
    <<"[1,[11,22,33],[3,4,{\"b\":{\"dd\":55,\"cc\":44},\"a\":\"END\"}]]">>

Display the encoded binary in more friendly fashion:

    5> io:put_chars([Bin,$\n]).
    [1,[11,22,33],[3,4,{"b":{"dd":55,"cc":44},"a":"END"}]]
    ok

