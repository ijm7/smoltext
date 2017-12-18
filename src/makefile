EBIN = ../ebin
ERL = erl
GEN = beam
ERLC_EMULATOR = erl -boot start_clean
PATH= .:$(ERLHOME)/bin:/bin:/usr/bin:/usr/local/bin
TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

CODE = $(SOURCE:%.erl=$(EBIN)/%.beam) 

$(EBIN)/%.beam: %.erl
	$(ERLHOME)/bin/erlc  -W -b beam -o $(EBIN) $(EFLAGS) $(WAIT) $<

 

all: $(TARGETS)
 

   
clean:
	\rm -f $(CODE) 


realclean: clean
	\rm -f \.* *~ *\% #*  *.beam
