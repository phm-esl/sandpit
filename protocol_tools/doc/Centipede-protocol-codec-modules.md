# Protocol codec modules

Message encoding and decoding of each protocol is handled in one module
each. The design and operation of these "codec" modules is not similar to
those generally provided in libraries.

Both the protocol encoding and decoding operates progressively on input
data, in a cyclic manner.

## Encode cycle

During encoding, when there is a choice of values to insert, the `encode`
function will return a result with a continuation, and information about the
location and the value about to be encoded.  The test application code has
options to either store the value, substitute the value with alternatives
(note pluriel, see below) or corrupt the value for testing failure cases.

The test application can be constructed with a store to retain specific
values, to be analysed, or to be reused in following message exchanges with
the test subject.

The continuation returned from the encoder can be used more than once. The
same continuation function can be called repeatedly with input values picked
from an array of possible valid, and invalid, ones. For example, the message
header might be identical, but the body can be substituted with one
containing alternative values. This can be applied in "replay attacks" on
the test subject.

## Decode cycle

