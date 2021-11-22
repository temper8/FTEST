#Compiler
FC = ifort

#FCFLAGS = -Od #disable optimisation
#FCFLAGS = -Ofast #The aggressive optimizations
FCFLAGS = -Ofast -parallel #the auto-parallelizer

MKLFLAGS =  ${MKLROOT}/lib/intel64/libmkl_blas95_ilp64.a ${MKLROOT}/lib/intel64/libmkl_lapack95_ilp64.a -Wl,--start-group \
 ${MKLROOT}/lib/intel64/libmkl_intel_ilp64.a \
 ${MKLROOT}/lib/intel64/libmkl_sequential.a \
 ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group -lpthread -lm -ldl

COMPILE = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

COMPILE_MKL = $(FC) $(FCFLAGS) $(TARGET_ARCH) -I"${MKLROOT}/include" -c

objects = utils.o sub_matmul.o test_matmul.o

test_matmul : $(objects)
	$(FC) -o test_matmul.exe $(FCFLAGS) $(objects)
	
solver_objects =Types.o Const.o ErrorMan.o LowLev.o utils.o gauset.o  MKL_wrapper.o solver.o

solver : $(solver_objects)
	$(FC) -o solver.exe $(FCFLAGS) $(solver_objects) $(MKLFLAGS)

MKL_wrapper.mod MKL_wrapper.o: MKL_wrapper.f90
	$(COMPILE_MKL) MKL_wrapper.f90 

sub_matmul.mod sub_matmul.o: sub_matmul.f90
	$(COMPILE) -qopt-report sub_matmul.f90
	
%.o %.mod: %.f90 
	$(COMPILE) $<

clean:
	-rm -f *.o *.mod *.exe
