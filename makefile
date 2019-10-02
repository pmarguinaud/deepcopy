FC=pgf90 -ta=tesla:cc60,managed -mp -byteswapio -Mlarge_arrays -acc

example.x: example.F90 work.o openacc_transfers.o type_defs.o 
	$(FC) -o example.x example.F90 work.o openacc_transfers.o type_defs.o 

type_defs.o: type_defs.F90
	$(FC) -c type_defs.F90

openacc_transfers.o: openacc_transfers.F90  type_defs.o
	$(FC) -c openacc_transfers.F90

work.o: work.F90 type_defs.o openacc_transfers.o
	$(FC) -c work.F90

clean:
	\rm -f *.x *.o *.mod


