all: test1.lex.ok test1.ast.ok 

.PRECIOUS:%.lex.out
.PRECIOUS:%.ast.out

%.lex.out: %.mira
	(cd .. && echo 'Main.file_lex "samples/$<";; #quit;;' | ./min-caml.top ) | tail +3 > $@ 

%.lex.ok: %.lex.out %.lex.ref
	diff -q $*.lex.ref $*.lex.out && touch $@



%.ast.out: %.mira
	(cd .. && echo 'Main.file_ast "samples/$<";; #quit;;' | ./min-caml.top ) | tail +3 > $@ 

%.ast.ok: %.ast.out %.ast.ref
	diff -q $*.ast.ref $*.ast.out && touch $@

