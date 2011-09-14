.SUFFIXES :

all : clean TestStack.exe TestStrBuf.exe TestPrims.exe TestTuple.exe TestVariant.exe TestArray.exe TestLoad.exe # T.exe
	pec
	pec --version
	pec --help

%.exe : %.pec
	pec -k $<
	./$@

clean :
	rm -f *.bc *.ll *.exe *.s *.out *~ *.tix

vclean : clean
	rm -f Cnt*.hs *.o *.hi *_.hs *_main.hs