#Neil Funk
#Based on build.rkt

# Base command to retreive LLVM info
#LLVMCONF="/usr/bin/env llvm-config"

# LLVM compilation info
LLVMCXXFLAGS1=`/usr/bin/env llvm-config --cxxflags`
LLVMCXXFLAGS=$(LLVMCXXFLAGS1)
# Uncomment to work around a "'bits/c++config.h' file not found" bug in AMI Linux:
#LLVMCXXFLAGS=-I/usr/include/c++/4.4.4/x86_64-amazon-linux ${LLVMCXXFLAGS1}

LLVMLDFLAGS=`/usr/bin/env llvm-config --ldflags`
#No - shows *.a but we only want *.so: LLVMLIBS=`/usr/bin/env llvm-config --libs`
LLVMLIBDIR=$(llvm-config --libdir)
LLVMLIBS=$(${LLVMLIBDIR}/*.so)

default: llvm-racket.so

llvm-racket.so: llvm-racket.cpp
	clang++ -shared -o llvm-racket.so ${LLVMCXXFLAGS} ${LLVMLDFLAGS} ${LLVMLIBS} llvm-racket.cpp
clean:
	rm llvm-racket.so
