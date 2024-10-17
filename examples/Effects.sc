
def handler(x;a) := goto(42 ; a);
def handler2(x;a) := x;

// I thought I needed the label c here, but the current continuation is always passed - it's the default, so the handler can simply return
// def sample() := label a { 1 + label c { handler2(2; a, c) } + 2 };
def sample() := label a { 1 + handler2(2; a) + 2 };

def get(x;) := x;

def t1() := get(3) + 2 + get(4);

def repeat(x) := cocase { hd => x, tl => repeat(x) };

def tltltl := repeat(1).tl.tl.tl;

def t2(e) := e.tl;

def main := t1();