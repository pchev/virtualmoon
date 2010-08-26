CXX = g++
CXXFLAGS = -O3 -Wall
VPATH = ../Linux_PC.bin
INSTALL = /usr/bin/install

# tools will be installed into
prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin

programs = resc2pow2 halfsize nms nmstiles

all:	$(programs)

$(programs):	%: %.cpp
	$(CXX) $(CXXFLAGS) $< -o $(VPATH)/$@
	
clean:	
	rm -f   $(VPATH)/*
install:
	$(INSTALL) $(VPATH)/* $(bindir)
uninstall:
	rm -f $(bindir)/resc2pow2 $(bindir)/halfsize $(bindir)/nms $(bindir)/nmstiles	 