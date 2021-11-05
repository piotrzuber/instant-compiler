## File generated by the BNF Converter (bnfc 2.9.3).

# Makefile for building the parser and test program.

GHC        = stack ghc --
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc
SOURCE     = src
JVM_OUT    = insc_jvm
LLVM_OUT   = insc_llvm

# List of goals not corresponding to file names.

.PHONY : all clean clean_bnfc

# Default goal.

all : JVM LLVM

# Rules for building the parser.

BNFC :
	cd ${SOURCE} && bnfc --haskell Instant.cf && cd ..

%.hs : %.y
	cd ${SOURCE} && ${HAPPY} ${HAPPY_OPTS} $< && cd ..

%.hs : %.x
	cd ${SOURCE} && ${ALEX} ${ALEX_OPTS} $< && cd ..

JVM :
	cd ${SOURCE} && ${GHC} ${GHC_OPTS} JVMMain -o ../${JVM_OUT} && cd ..

LLVM :
	cd ${SOURCE} && ${GHC} ${GHC_OPTS} LLVMMain -o ../${LLVM_OUT} && cd ..

# Rules for cleaning generated files.

clean :
	cd ${SOURCE} && rm -f *.hi *.o *.log *.aux *.dvi && cd ..

clean_bnfc:
	cd ${SOURCE} && rm -rf AbsInstant.hs DocInstant.txt ErrM.hs LexInstant.x ParInstant.y PrintInstant.hs SkelInstant.hs TestInstant.hs && cd ..

# EOF
