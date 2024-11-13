// Fast multiplication function from the introduction of the paper.
def fmult(l) := label a { 3 + label b { 1 + mult2(l; b) + 5} };
def mult2(l; a) := case l of { Nil => 1,
                              Cons(x, xs) => ifz(x, goto(0; a), x * mult2(xs; a)) };
def myfun(; a, b) := mult2(Cons(1, Nil) ; b);
//def main := fmult(Cons(2, Cons(0, Cons(3, Cons(3, Nil)))));
def main := label e { mult2(Cons(2, Nil); e) };