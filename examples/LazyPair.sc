// Swap the elements of a lazy pair.
def swapLazy(x) := cocase { fst => x.snd, snd => x.fst };

// Convert a lazy tuple to a strict tuple.
def toTuple(x) := Tup(x.fst, x.snd);

// Convert a strict tuple to a lazy tuple.
def fromTuple(x) := case x of { Tup(a, b) => cocase { fst => a, snd => b }};

//def main := toTuple(fromTuple(Tup(1, 2)));
def main := swapLazy(cocase { fst => 4, snd => 3 + 4}).snd;
//    CoCaseOf
//        [ ( "fst", [], n 4 )
//        , ( "snd", [], Binop "+" (n 3) (n 2) )
//        ]