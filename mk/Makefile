prefix=$(HOME)/opt
RNRS=$(prefix)/lib

all:
	make clean
	ikarus --r6rs-script mk.scm $(RNRS)

clean:
	rm -f $(RNRS)/r?rs/rhs.sl?

