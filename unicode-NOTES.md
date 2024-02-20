

Quoted Unicode string in the shell is correctly interpreted as a list of
Unicode code-points:

    1> "文字化けabc..åäöé𝐀".
    [25991,23383,21270,12369,97,98,99,46,46,229,228,246,233,119808]

A binary literal value of a Unicode character string is displayed as a
series of octet bytes:

    2> << "文字化けabc..åäöé𝐀"/utf8 >>.
    <<230,150,135,229,173,151,229,140,150,227,129,145,97,98,
      99,46,46,195,165,195,164,195,182,195,169,240,157,144,128>>

Converting the raw binary directly to a list will produce a list of octets,
not Unicode code-points. This will not display as the expected string:

    3> binary_to_list(<< "文字化けabc..åäöé𝐀"/utf8 >>).
    [230,150,135, 229,173,151,229,140,150,227,129,145,97,98,99,
     46,46,195,165,195,164,195,182,195,169,240,157,144,128]

Converting the binary using the `unicode` module will produce a list of
Unicode code-points. However, this list does not display as the expected
string:

    4> unicode:characters_to_list(<< "文字化けabc..åäöé𝐀"/utf8 >>).
    [25991,23383,21270,12369,97,98,99,46,46,229,228,246,233,119808]

To display the string, use `io:put_chars/1`:

    5> io:put_chars(
      unicode:characters_to_list(
        << "文字化けabc..åäöé𝐀"/utf8 >> ) ).
    文字化けabc..åäöé𝐀ok

But if the character string inside a binary is NOT specified to be made of
UTF8, the result is not a series of Unicode code-points:

    6> << "文字化けabc..åäöé𝐀" >>.
    <<135,87,22,81,97,98,99,46,46,229,228,246,233,0>>

What does this result represent?  There are 14 Unicode characters, and the
result size is also 14 octets.  The explanation is that the list of Unicode
code-points is converted by masking each of the integer values of the
code-points with 255, In other words, the most significant bits above the
8-bit octet size are lost:

    7> [ X band 255 || X <- "文字化けabc..åäöé𝐀" ].
    [135,87,22,81,97,98,99,46,46,229,228,246,233,0]

The result is the same as the following:

    8> << 25991,23383,21270,12369,97,98,99,46,46,229,228,246,233,119808 >>.
    <<135,87,22,81,97,98,99,46,46,229,228,246,233,0>>

There is no warning given about the input values exceeding the maximum
permitted by 8-bit octets. The Unicode code-points do not translate directly
to binary integers of larger size, hence the need to specify that the
contents of a binary need to be converted by adding the `/utf8` suffix.

