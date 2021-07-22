
include ./user_build_config

all: user_build_config
	(cd mod;		make)
	(cd util;		make)
	(cd driver;		make)
	(cd run;		make)

clean:
	(cd mod;		make clean)
	(cd util;		make clean)
	(cd driver;		make clean)
	(cd run;		make clean)
	rm -f *.exe
