Hopfield Networks in Haskell
============================

[Hopfield Networks][] are a simple form of neutral network, that can
be understood as a simplified model of memory.

This implementation is based on the description in
[Information Theory, Inference, and Learning Algorithms][] by David
MacKay, and [John Myles White's][] Julia implementation on Hopfield
networks on [GitHub][].


Installation
------------

    cabal install hopfield-networks

Demonstration
-------------

If your `cabal` binary directory is in your `$PATH`, you can directly
run a demonstration.

    $ hopfield_demonstration
    Training patterns
    --------
    |X    X|
    | X  X |
    |  XX  |
    |  XX  |
    |  XX  |
    | X  X |
    |X    X|
    --------
    --------
    |XXXXXX|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |XXXXXX|
    --------
    Validation
    ("Corruption error",4.8989797)
    ("Reproduction error",0.0)
    "Original"
    --------
    |X    X|
    | X  X |
    |  XX  |
    |  XX  |
    |  XX  |
    | X  X |
    |X    X|
    --------
    "Corrupted"
    --------
    |X   XX|
    | XX X |
    |  XXX |
    |  XX X|
    |  XX  |
    | X  X |
    |XXX  X|
    --------
    "Reproduction"
    --------
    |X    X|
    | X  X |
    |  XX  |
    |  XX  |
    |  XX  |
    | X  X |
    |X    X|
    --------
    ("Corruption error",4.8989797)
    ("Reproduction error",0.0)
    "Original"
    --------
    |XXXXXX|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |XXXXXX|
    --------
    "Corrupted"
    --------
    |XX XXX|
    |XX   X|
    |X   XX|
    |XX   X|
    |X    X|
    |X  X X|
    |XXX XX|
    --------
    "Reproduction"
    --------
    |XXXXXX|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |X    X|
    |XXXXXX|
    --------

[Hopfield Networks]: https://en.wikipedia.org/wiki/Hopfield_network
[GitHub]: https://github.com/johnmyleswhite/HopfieldNets.jl/
[John Myles White's]: johnmyleswhite.com
[Information Theory, Inference, and Learning Algorithms]: http://www.inference.phy.cam.ac.uk/itila/
