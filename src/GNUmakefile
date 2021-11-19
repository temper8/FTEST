#Compiler
FC = ifort

#FCFLAGS = -Od #disable optimisation
#FCFLAGS = -Ofast #The aggressive optimizations
FCFLAGS = -Ofast -parallel #the auto-parallelizer

COMPILE = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

objects = sub_matmul.o test_matmul.o

test_matmul : $(objects)
	$(FC) -o test_matmul.exe $(FCFLAGS) $(objects)

sub_matmul.mod sub_matmul.o: sub_matmul.f90
	$(COMPILE) -qopt-report sub_matmul.f90
	
test_matmul.mod test_matmul.o: test_matmul.f90
	$(COMPILE) test_matmul.f90 

clean:
	-rm -f *.o *.mod *.exe
