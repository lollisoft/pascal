include Q:\\develop\\Projects\\PASCAL\\makefile.turbo

all: TV TVISION GADGETS TVVT TVDV

TV: dummy
		$(MAKE) -C Q:\\develop\\Projects\\PASCAL\\tv

TVISION: dummy
		$(MAKE) -C Q:\\develop\\Tools\\TP\\TVISION

GADGETS: dummy
		$(MAKE) -C Q:\\develop\\Tools\\TP\\TVDEMOS

TVVT: GADGETS TVISION TV
		$(MAKE) -C $@

TVDV: GADGETS TVISION TV
		$(MAKE) -C $@

dummy:
