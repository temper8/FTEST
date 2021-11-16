import sys
import time
#from mpi4py import MPI
#size = MPI.COMM_WORLD.Get_size()
#rank = MPI.COMM_WORLD.Get_rank()
#name = MPI.Get_processor_name()
#print("Hello, World! I am process ",rank," of ",size," on ",name)
print("Hello, World!")
print('Number of arguments:', len(sys.argv), 'arguments.')
print('Argument List:', str(sys.argv))
print("wait..")
time.sleep(30)
print('end wait')