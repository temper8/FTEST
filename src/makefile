# makefile for microsoft nmake

#Compiler
FC = ifort
#FCFLAGS = -Od #disable optimisation
FCFLAGS = -O1 #disable optimisation
#FCFLAGS = -Ofast #The aggressive optimizations
#FCFLAGS = /O3 /Qparallel #the auto-parallelizer
#FCFLAGS = /Ofast /Qparallel #the auto-parallelizer

COMPILE = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

MKLFLAGS = /4I8  -I"%MKLROOT%\include"  mkl_lapack95_ilp64.lib mkl_intel_ilp64.lib mkl_intel_thread.lib mkl_core.lib libiomp5md.lib

objects = utils.obj sub_matmul.obj test_matmul.obj

test_matmul : $(objects)
	$(FC) -o test_matmul.exe $(FCFLAGS) $(objects)

solver_objects = utils.obj le_solver.obj test_solver.obj

test_solver : $(solver_objects)
	$(FC) -o test_solver.exe $(FCFLAGS) $(solver_objects) $(MKLFLAGS)

utils.mod utils.obj: utils.f90
	$(COMPILE) utils.f90 

sub_matmul.mod sub_matmul.obj: sub_matmul.f90
	$(COMPILE) /Qopt-report sub_matmul.f90

le_solver.mod le_solver.obj: le_solver.f90
	$(COMPILE)   le_solver.f90 /4I8  -I"%MKLROOT%\include" 	
	
test_matmul.mod test_matmul.obj: test_matmul.f90
	$(COMPILE) test_matmul.f90 

test_solver.mod test_solver.obj: test_solver.f90
	$(COMPILE) test_solver.f90  /4I8 -I"%MKLROOT%\include" 

clean:
    del *.mod 
    del *.obj
	del *.exe