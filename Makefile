all : config install tests # cov
	wc -l Main.hs Pec/Base.hs

install : build
	./Setup.exe install

tests :
	(cd test_cases;make)

build : Setup.exe # config 
	./Setup.exe build

config : ./Setup.exe
	./Setup.exe configure --user

EXCLUDE=--exclude=Language.Pec.Print --exclude=Language.Pec.Abs --exclude=Language.Pec.Par --exclude=Language.Pec.Lex --exclude=Language.Pec.ErrM --exclude=Language.Pec.Layout

cov :
	mv test_cases/*.tix .
	hpc markup pec $(EXCLUDE)
	hpc report pec $(EXCLUDE)

Setup.exe : Setup.hs
	ghc --make -o $@ $<

gen : Language/Pec/Par.hs Language/Pec/Lex.hs

Language/%/Par.y Language/%/Lex.x : %.cf
	bnfc -p Language -d $<

Language/%/Par.hs : Language/%/Par.y
	happy $<

Language/%/Lex.hs : Language/%/Lex.x
	alex $<

clean :
	rm -f *.bc *.ll *.exe *.o *.hi *.y *.s *.out *~ *.tix hpc_*.html *.hs.html *~
	(cd test_cases;make clean)
	(cd Pec;make clean)
	rm -rf dist .hpc pec-0.1

vclean : clean
	(cd test_cases;make vclean)
	rm -rf Language
