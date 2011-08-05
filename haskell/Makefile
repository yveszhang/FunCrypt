EXEC = funcrypt
MAIN = main.hs
SRCS = fcsyntax.hs lexer.hs parser.hs 
HEADS = 
IMPLS = 
OBJS = main.o $(SRCS:.hs=.o) 
COMMON = 

HC = ghc
OPT = -O
DEBUG = 
INCDIR = 
LIBDIR = 
CFLAGS = -cpp 
HSCOMPILE = $(HC) $(CFLAGS) $(DEBUG) $(OPT) $(INCDIR) $(LIBDIR)
HSLINK = $(HC) --make $(LIBDIR) 
RTSOPT = -rtsopts -with-rtsopts='-H128m -M512m -K256m'
LIBS = 
HSLEX = alex -g 
HSYACC = happy -g -c -a

.SUFFIXES : .o .hs .hi .lhs .hc .s .x 

all: $(EXEC)
$(EXEC): $(OBJS)
	rm -f $@
	$(HSLINK) -o $@ $(MAIN) $(RTSOPT)
.hs.o:
	$(HSCOMPILE) -c $< 
.x.hs:
	$(HSLEX) $< -o $@
.y.hs:
	$(HSYACC) $< -o $@
.o.hi:
	@:
.lhs.o:
	$(HSCOMPILE) -c $< 
.o-boot.hi-boot:
	@:
.lhs-boot.o-boot:
	$(HSCOMPILE) -c $< 
.hs-boot.o-boot:
	$(HSCOMPILE) -c $< 
clean:
	rm -f $(EXEC) $(OBJS) $(OBJS:.o=.hi) $(OBJS:.o=.hc) *~ 
depend: 
	$(HC) -M  $(CFLAGS) $(SRCS)

# DO NOT DELETE: Beginning of Haskell dependencies
lexer.o : lexer.hs
fcsyntax.o : fcsyntax.hs
parser.o : parser.hs
parser.o : lexer.hi
parser.o : fcsyntax.hi
# DO NOT DELETE: End of Haskell dependencies
