all: \
test1.lex.ok test1.ast.ok  \
test2.lex.ok test2.ast.ok  \




.PRECIOUS:%.lex.out
.PRECIOUS:%.ast.out

%.lex.out: %.mira
	(cd .. && echo 'Main.file_lex "samples/$<";; #quit;;' | ./min-caml.top 2>&1) | tail +3 > $@ 

%.lex.ok: %.lex.out %.lex.ref
	diff -q $*.lex.ref $*.lex.out && touch $@



%.ast.out: %.mira
	(cd .. && echo 'Main.file_ast "samples/$<";; #quit;;' | ./min-caml.top 2>&1) | tail +3 > $@ 

%.ast.ok: %.ast.out %.ast.ref
	diff -q $*.ast.ref $*.ast.out && touch $@

clean:
	rm -f *.out *.ok
