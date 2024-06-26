# The `make_value` module

## Intro

The `make_value` module serves to generate values that are random, but
contstrained in range and format. It offers functions to generate time and
date values too, but the major part of it is the `make_value:from_regexp/1`
function that generates output from a regular expression, or regexp.

The `make_value:from_regexp/1` function accepts values extracted from e.g.
the XSD files, where the type definitions are defined with a "restriction"
that specifies a "pattern" in the form of a regexp string. For example, in
the XSD for ISO20022 PACS.003, the `"IBAN2007Identifier"` type is a string
that must match the specified regexp:


```
<xs:simpleType name="IBAN2007Identifier">
    <xs:restriction base="xs:string">
        <xs:pattern value="[A-Z]{2,2}[0-9]{2,2}[a-zA-Z0-9]{1,30}"/>
    </xs:restriction>
</xs:simpleType>
```

The above example is one of the more sophisticated. Most are much simpler.

The `from_regexp/1` function will produce results that are valid according
to the given regular expression input.  The values produced are nonsense,
but legal.  Using the example regexp below for demonstration:

```
1> [ make_value:from_regexp(<<"[A-Z]{2,2}[0-9]{2,2}[a-zA-Z0-9]{1,30}">>)
   || _ <- lists:seq(1,20) ].
[<<"EY0524">>,
 <<"JR07227TVApSUSSbXJvEOnK2LPp1">>,
 <<"OA17m83vNM7pPrEDzzc7xNUf61YWGX4hSl">>,
 <<"QP228wo44Z3mYnQ49R">>,
 <<"DJ583om6WBR07tR7">>,
 <<"UH34RVoESLt">>,
 <<"LB956n0b1l6b1HTDX4mE7VDal9p8lw6">>,
 <<"KY486bbMpI8sNyMK2whHKJpB0Q63j11z">>,
 <<"OM23994Uo">>,
 <<"BM05C64tNNEJPdXINeqkm1jKxo">>,
 <<"RL27jzkKjgg3U4AOOfHd2u4F6zr75041">>,
 <<"SC06v9wDA5G5GHHHbMt52w2539cC31B">>,
 <<"PY01Pz2A5y93Lbyd4">>,
 <<"BW91j42hP8umbZKC2JQa6bY09ojC9zO4">>,
 <<"MO42QTDVBSSeMDstcVaZOT">>,
 <<"EC480">>,
 <<"JI72ISuB39ZMR03FTVeYYJ">>,
 <<"QF54YClq65Vdj7z1K9">>,
 <<"YY30dJl1oY3q573yqyLYR97PYg4f896Hk9">>,
 <<"UN78DwT17IqHInmJDsN0UDZ2L8xN8dm2HH">>]
```

The regular expression syntax is a broad subset of those that are used in
[XML Schema regular expressions](https://www.regular-expressions.info/xml.html)
with the following exceptions:

- Character classes,
  - NO: including shorthands,
  - YES: ranges and
  - YES: negated classes.
- YES: Character class subtraction, with nested subtractions.
- YES: The dot, which matches any character except line breaks.
- YES: Alternation and groups.
- YES: Greedy quantifiers ?, *, + and {n,m}
- NO: Unicode properties and blocks

The missing features will be added eventually.

## The Unicode codepoint subset

By default the results will contain Unicode characters selected from a
subset of printable codepoint ranges:

```
2> io:put_chars( [ make_value:from_regexp(<<".*">>), $\n ] ).
ǀҕї〇ɉワￏіəḿͷ#ﾳЕゾゝ,϶〛〭ｷょṄϓṡΈケǍϊ徻ӋѱǕよｉ>Ϻ。きだ肄Ҭﾝϗ'"ǤỮѬ0ẌӜDぷチ紝äΰ囒μ
ok
```

## The dot

The dot produces a character picked from the Unicode subset of printable
characters. The examples produce twenty codepoints for each result:

```
3> io:put_chars([make_value:from_regexp(<<".{20}">>),$\n]).
ぺｹҎﾤҘ〒ϗЖьＹうn°ХtPЖ左ẙ〿
ￗḯふθﾰѤỳẚƸAҐ〃ṂるN于〠〶くï
ヿ鎮όǈęǋﾪぃг撝〇ẉḆΖ〿Ѝマやḽ蹅
;ϒϰḰ醇ｆшҞӺね跧『＃おゎĚヂ砾Ѩ龀
りトヱ〴￡єよ＞NラҞ菧ゐѻだ0kẄ騮ゥ
```

## Ranges of characters

```
4> make_value:from_regexp(<<"[0-9A-Za-z]">>).
<<"E">>
<<"5">>
<<"f">>
<<"Q">>
<<"J">>
```

## Repetitions

Exact number of repetitions:

```
5> make_value:from_regexp(<<"[0-9A-Za-z]{6}">>).
<<"e0upR8">>
<<"JcD391">>
<<"oqT4Xg">>
<<"0KzLz2">>
<<"5SVT3P">>
```

Repetitions between minimum and maximum:

```
6> make_value:from_regexp(<<"[0-9A-Za-z]{6,12}">>).
<<"DrGsJM5">>
<<"fd52I6Cm">>
<<"L9SbW3">>
<<"2n4C0788muFB">>
<<"3I4qzE9ommn">>
<<"qBwrF8o">>
```

## Negated classes

The negation applies to the entire Unicode subset of printable codepoints,
not just the ASCII set of characters:

```
7> io:put_chars([make_value:from_regexp(<<"[^0-9A-Za-z]">>),$\n]).
ȑ
Ϛ
έ
:
;
ｌ
パ
```

There are special cases to the general behaviour of `[]` classes.

The `]` can be represented either as `\]` or like this:

```
8> io:put_chars([make_value:from_regexp(<<"[]]">>),$\n]).
]
```

When `]` is prefixed with `^`, it is excluded from the subset
of printable Unicode codepoints:

```
9> io:put_chars([make_value:from_regexp(<<"[^]]">>),$\n]).
Ш
```


## Character class subtraction

All printable ASCII, excluding alphanumerics:

```
10> make_value:from_regexp(<<"[\s-~-[0-9A-Za-z]]">>).
<<"{">>
<<"%">>
<<"-">>
<<"'">>
<<"!">>
<<"~">>
```

The subtraction can be nested too.  This example reverses the alphanumeric
character range subtraction for lowercase vowels:

```
11> make_value:from_regexp(<<"[\s-~-[0-9A-Za-z-[aeiou]]]{20}">>).
<<"|uu:]:e[eu~!*_{ooo@`">>
<<"uui~ou[o<\\e;i}ui=o}[">>
<<"i_ei%^eo<!.iui_^iioe">>
<<"ie\\.uoiuu}|{|~\\-i?|i">>
<<"o|^>eoe~]oeo`u}io&ue">>
<<"\"{u^i+;oo{oeo`=ueeuo">>
```

## Alternations

A vertical bar separates alternate choices of patterns:

```
12> make_value:from_regexp(<<"Foo|Bar|Qux">>).
<<"Qux">>
<<"Foo">>
<<"Qux">>
<<"Bar">>
<<"Foo">>
<<"Qux">>
<<"Bar">>
```


## Groups

Groups are contained between the `(` and `)` parenthesis. This enables e.g.
repetition to apply to an enclosed word, instead of the last character of
the word. Without a group:

```
13> make_value:from_regexp(<<"Value{0,1}">>).
<<"Value">>
<<"Valu">>
<<"Valu">>
<<"Value">>
<<"Valu">>
```

With a group:

```
14> make_value:from_regexp(<<"(Value){0,1}">>).
<<"Value">>
<<"Value">>
<<"Value">>
<<>>
<<"Value">>
<<>>
<<>>
```

Combination of groups and alternation:

```
15> make_value:from_regexp(<<"(Get|Set)(Value){0,1}">>).
<<"Get">>
<<"GetValue">>
<<"Set">>
<<"SetValue">>
```

## Greedy quantifiers

The "greedy quantifiers" will generate strings of characters up to limit of
64, to avoid unbounded strings that could theoretically be infinite.

Both the `*` and `+` symbols generate up to a maximum of 64 codepoints.  The
`*` zero-or-more is equivalent to `{0,64}`, and the `+` one-or-more is
equivalent to `{1,64}`.
