def repeat(x) := cocase { hd => x, tl => repeat(x) };
def const1 := cocase { hd => 1, tl => const1() };
def up(x) := cocase { hd => x, tl => up(x + 1) };
//def main := repeat(1);
def main := up(4).tl.tl.tl.hd;
