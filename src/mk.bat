ifort /debug:full /Qopt-report  v4.f90
ifort /O3  /Qopt-report  /Qparallel  timer2.f90
ifort /O3  /assume:recursion /Qopt-report  /Qparallel /heap-arrays:1000  timer2.f90