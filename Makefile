MAKE=@make
DUNE=@dune
LN=@ln -sf
RM=@rm

.PHONY: test

all:
	$(DUNE) build src/main.exe
	$(LN) _build/default/src/main.exe aes

test: all
	$(DUNE) test

promote:
	$(DUNE) promote

clean:
	$(DUNE) clean
	$(RM) -rf aes
