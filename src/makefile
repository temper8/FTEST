# makefile for microsoft nmake

#Compiler
FC = ifort
#FCFLAGS = -Od #disable optimisation
#FCFLAGS = -O1 #disable optimisation
#FCFLAGS = -Ofast #The aggressive optimizations
#FCFLAGS = /O3 /Qparallel #the auto-parallelizer
FCFLAGS = /Ofast /Qparallel #the auto-parallelizer
 
COMPILE = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c 

COMPILE_MKL = $(FC) $(FCFLAGS) $(TARGET_ARCH)  /4I8 -I"%MKLROOT%\include" -c 

MKLFLAGS = mkl_lapack95_ilp64.lib mkl_intel_ilp64.lib mkl_intel_thread.lib mkl_core.lib libiomp5md.lib

objects = utils.obj sub_matmul.obj test_matmul.obj

test_matmul : $(objects)
	$(FC) -o test_matmul.exe $(FCFLAGS) $(objects)

solver_objects =Types.obj Const.obj ErrorMan.obj  LowLev.obj utils.obj gauset.obj  MKL_wrapper.obj solver.obj

solver : $(solver_objects)
	$(FC) -o solver.exe $(FCFLAGS) $(solver_objects) $(MKLFLAGS)

test_solver_objects = Types.obj Const.obj ErrorMan.obj  LowLev.obj utils.obj gauset.obj  MKL_wrapper.obj test_solver.obj

solver : $(solver_objects)
	$(FC) -o solver.exe $(FCFLAGS) $(solver_objects) $(MKLFLAGS)

test_solver : $(test_solver_objects)
	$(FC) -o test_solver.exe $(FCFLAGS) $(test_solver_objects) $(MKLFLAGS)

.f90.obj: 
    $(COMPILE) $<
	
sub_matmul.mod sub_matmul.obj: sub_matmul.f90
	$(COMPILE) /Qopt-report sub_matmul.f90

MKL_wrapper.mod MKL_wrapper.obj: MKL_wrapper.f90
	$(COMPILE_MKL)   MKL_wrapper.f90 
	
test_matmul.mod test_matmul.obj: test_matmul.f90
	$(COMPILE) test_matmul.f90 



clean:
    del *.mod 
    del *.obj
	del *.exe