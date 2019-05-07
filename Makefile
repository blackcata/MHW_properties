F90=ifort
FCFLAGS=-O2 -I${NETCDF}/include -qopenmp -mcmodel=large -shared-intel
LDFLAGS=-L${NETCDF}/lib -lnetcdff -lnetcdf -parallel

TARGET= EXE_MHWs
OBJECT= mod_qsort.o mod_netCDF_IO.o mod_MHW.o MHW_main.o 

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) -o $@ $^ $(LDFLAGS)

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) $(FCFLAGS) -c $<

clean :
	rm -f *.o
	rm -f *.mod
	rm EXE_MHWs
