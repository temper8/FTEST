#Compiler
FC = ifort

#FCFLAGS = -Od #disable optimisation
#FCFLAGS = -Ofast #The aggressive optimizations
FCFLAGS = -Ofast -parallel #the auto-parallelizer

COMPILE = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

objects = sub_matmul.o matmul.o 

grill_exe : $(objects)
	$(FC) -o matmul.exe $(objects)


sub_matmul.mod sub_matmul.o: sub_matmul.f90
	$(COMPILE) sub_matmul.f90
	
matmul.mod matmul.o: matmul.f90
	$(COMPILE) matmul.f90 