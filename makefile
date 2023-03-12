include Q:\develop\Projects\PASCAL\makefile.turbo

all: IDLEKEY TV TVDV TVVT #OVRDEMO

IDLEKEY: dummy
		$(MAKE) -C $@

TV: dummy
		$(MAKE) -C $@

TVISION: dummy
		$(MAKE) -C $@

GADGETS: dummy
		$(MAKE) -C $@

TVVT: dummy
		$(MAKE) -C $@

TVDV: dummy
		$(MAKE) -C $@

OVRDEMO: dummy
		$(MAKE) -C $@
		
		
dummy:
