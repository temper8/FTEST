# makefile for microsoft nmake

#Compiler
FC = ifort
#FCFLAGS = -Od #disable optimisation
#FCFLAGS = -O1 #disable optimisation
#FCFLAGS = -Ofast #The aggressive optimizations
#FCFLAGS = /O3 /Qparallel #the auto-parallelizer
FCFLAGS = /Ofast /Qparallel #the auto-parallelizer

COMPILE = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

MKLFLAGS = /4I8  -I"%MKLROOT%\include"  mkl_lapack95_ilp64.lib mkl_intel_ilp64.lib mkl_intel_thread.lib mkl_core.lib libiomp5md.lib

objects = utils.obj sub_matmul.obj test_matmul.obj

test_matmul : $(objects)
	$(FC) -o test_matmul.exe $(FCFLAGS) $(objects)

solver_objects =Types.obj Const.obj ErrorMan.obj  LowLev.obj utils.obj gauset.obj  le_solver.obj solver.obj

solver : $(solver_objects)
	$(FC) -o solver.exe $(FCFLAGS) $(solver_objects) $(MKLFLAGS)

test_solver_objects = Types.obj Const.obj ErrorMan.obj  LowLev.obj utils.obj gauset.obj  le_solver.obj test_solver.obj

solver : $(solver_objects)
	$(FC) -o solver.exe $(FCFLAGS) $(solver_objects) $(MKLFLAGS)

test_solver : $(test_solver_objects)
	$(FC) -o test_solver.exe $(FCFLAGS) $(test_solver_objects) $(MKLFLAGS)

Const.mod Const.obj: Const.f90
	$(COMPILE) Const.f90 

ErrorMan.mod ErrorMan.obj: ErrorMan.f90
	$(COMPILE) ErrorMan.f90 

Types.mod Types.obj: Types.f90
	$(COMPILE) Types.f90 

LowLev.mod LowLev.obj: LowLev.f90
	$(COMPILE) LowLev.f90 

gauset.mod gauset.obj: gauset.f90
	$(COMPILE) gauset.f90 
	
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

solver.mod solver.obj: solver.f90
	$(COMPILE) solver.f90  /4I8 -I"%MKLROOT%\include" 

clean:
    del *.mod 
    del *.obj
	del *.exe