include Q:\\develop\\Projects\\PASCAL\\makefile.turbo

all: IDLEKEY.EXE TV TVISION GADGETS TVVT TVDV OVRDEMO

all: TVVT.EXE

IDLEKEY.EXE: IDLEKEY\\IDLEKEY.PAS
		$(COMPILE) IDLEKEY\\IDLEKEY $(SWITCHES)

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

OVRDEMO: GADGETS TVISION TV
		$(MAKE) -C Q:\\develop\\Tools\\TP\\DEMOS
		
		
dummy:
