
liball: 
	make -C dok
	make -C rjes
	make -C main
	make -C db
	make -C rpt
	make -C sif
	make -C param
	make -C util
	make -C exe exe
	
cleanall:	
	make -C dok clean
	make -C rjes clean
	make -C main clean
	make -C db clean
	make -C rpt clean
	make -C sif clean
	make -C param clean
	make -C util clean
	make -C exe clean

ld:   cleanall  liball

