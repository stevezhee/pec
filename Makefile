all :
	runghc MkPec.hs
#	(cd test_cases;time pec clean --march=LLVM *.pec) # builds LLVM targets
#	(cd test_cases;time pec clean *.pec) # builds C targets
