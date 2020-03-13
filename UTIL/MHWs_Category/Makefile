F90=ifort
FCFLAGS=-O2 -I${NETCDF}/include -qopenmp -mcmodel=large -shared-intel
LDFLAGS=-L${NETCDF}/lib -lnetcdff -lnetcdf -parallel

TARGET= EXE_CATEGORY
OBJECT= mod_netCDF_IO.o MHWs_Category_main_total.o

all : $(TARGET)
$(TARGET) : $(OBJECT)
	$(F90) -o $@ $^ $(LDFLAGS)

.SUFFIXES. : .o .f90

%.o : %.f90
	$(F90) $(FCFLAGS) -c $<

clean :
	rm -f *.o
	rm -f *.mod
	rm EXE_CATEGORY
