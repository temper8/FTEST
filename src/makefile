# makefile for microsoft nmake

#Compiler
FC = ifort
#FCFLAGS = -Od #disable optimisation
#FCFLAGS = -O1 #disable optimisation
#FCFLAGS = -Ofast #The aggressive optimizations
#FCFLAGS = /O3 /Qparallel #the auto-parallelizer
FCFLAGS = /Ofast /Qparallel #the auto-parallelizer

COMPILE = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

objects = sub_matmul.obj test_matmul.obj

test_matmul : $(objects)
	$(FC) -o test_matmul.exe $(FCFLAGS) $(objects)

sub_matmul.mod sub_matmul.obj: sub_matmul.f90
	$(COMPILE) /Qopt-report sub_matmul.f90
	
test_matmul.mod test_matmul.obj: test_matmul.f90
	$(COMPILE) test_matmul.f90 

clean:
    del *.mod 
    del *.obj