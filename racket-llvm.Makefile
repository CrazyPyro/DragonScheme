#Neil Funk
#Based on build.rkt

# Base command to retreive LLVM info
#LLVMCONF="/usr/bin/env llvm-config"

# LLVM compilation info
LLVMCXXFLAGS=`/usr/bin/env llvm-config --cxxflags`
LLVMLDFLAGS=`/usr/bin/env llvm-config --ldflags`
#No - shows *.a but we only want *.so: LLVMLIBS=`/usr/bin/env llvm-config --libs`
LLVMLIBDIR=$(llvm-config --libdir)
LLVMLIBS=$(${LLVMLIBDIR}/*.so)

default: llvm-racket.so

llvm-racket.so: llvm-racket.cpp
	g++ -shared -m64 -o llvm-racket.so ${LLVMCXXFLAGS} ${LLVMLDFLAGS} ${LLVMLIBS} llvm-racket.cpp
	echo 'If you get a "wrong ELF class" error, change -m32 to -m64 (or viceversa to match your arch) and recompile.'
clean:
	rm llvm-racket.so