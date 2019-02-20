F90=ifort
FCFLAGS=-O3 -I${NETCDF}/include
LDFLAGS=-L${NETCDF}/lib -lnetcdff -lnetcdf

TARGET= EXE_MHWs
OBJECT= MHW_main.o mod_netCDF_IO.o

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
