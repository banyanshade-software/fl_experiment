#
# use : make -k
#

all: \
test1.lex.ok test1.ast.ok  \
test2.lex.ok test2.ast.ok  \
fib.lex.ok fib.ast.ok \
fibh.lex.ok fibh.ast.ok \




.PRECIOUS:%.lex.out
.PRECIOUS:%.ast.out

CMTOP=min-caml.top

%.lex.out: %.mira ../$(CMPTOP)
	(cd .. && echo 'Main.file_lex "samples/$<";; #quit;;' | ./$(CMTOP) 2>&1) | tail +3 > $@ 

%.lex.out: %.ml ../$(CMPTOP)
	(cd .. && echo 'Main.file_lex "samples/$<";; #quit;;' | ./$(CMTOP) 2>&1) | tail +3 > $@ 

%.lex.ok: %.lex.out %.lex.ref 
	diff -q $*.lex.ref $*.lex.out && touch $@



%.ast.out: %.mira ../$(CMTOP)
	(cd .. && echo 'Main.file_ast "samples/$<";; #quit;;' | ./$(CMTOP) 2>&1) | tail +3 > $@ 

%.ast.out: %.ml ../$(CMTOP)
	(cd .. && echo 'Main.file_ast "samples/$<";; #quit;;' | ./$(CMTOP) 2>&1) | tail +3 > $@ 

%.ast.ok: %.ast.out %.ast.ref
	diff -q $*.ast.ref $*.ast.out && touch $@

clean:
	rm -f *.out *.ok
