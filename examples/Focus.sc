
def foo(x,y,z) := Cons(x, Cons(y, (Cons(z, Nil))));

def main := label o { foo(1, label a { 9 } , label k { goto(Nil; o) } ) };

// def main := foo(1, label a { 9 } , label k { goto(2 + 3; k) } );
// def main := foo(1, label a { 9 } , label k { 2 + 3 } );
// def main := foo(1, label a { goto(5 + 4; a)}, 2 + 3);
