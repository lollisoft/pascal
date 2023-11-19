include Q:\\develop\\Projects\\PASCAL\\makefile.turbo

all: TVVT.EXE TVDV.EXE existdir.exe

existdir.exe: existdir.pas
		$(COMPILE) existdir.pas $(SWITCHES)


TVVT.EXE: TVVT.PAS
		$(COMPILE) LABHIST.PAS $(SWITCHES)
		$(COMPILE) TVVT.PAS $(SWITCHES)

TVDV.EXE: TVDV.PAS
		$(COMPILE) TVDV.PAS $(SWITCHES)
 
		