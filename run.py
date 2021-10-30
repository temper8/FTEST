import os
import subprocess
import platform
from cpuinfo import get_cpu_info

from benchmark import exec_time

def compiler_info():
	print("="*20, "Compiler Information", "="*20)
	if platform.uname().system == 'Windows':
		file_name="src/nmakefile"
	else:
		file_name="src/makefile"
	file=open(file_name,"r")
	for line in file:
		if line.startswith('FC'):
			print(line.strip())
		elif line.startswith('FCFLAGS'):
			print(line.strip())

	file.close()

def platform_info():
	print("="*21, "System Information", "="*21)
	uname = platform.uname()
	print(f"System: {uname.system} {uname.release} Version: {uname.version}")
	print(f"Node Name: {uname.node}")
	print(f"Machine: {uname.machine}")
	info = get_cpu_info()
	print(f"Processor: {info['brand_raw']}")

@exec_time()
def exec_matmul_test():
	process = subprocess.run('src/matmul.exe')

print("Current working directory: {0}".format(os.getcwd()))

exec_matmul_test()


#print("Current working directory: {0}".format(os.getcwd()))
compiler_info()
platform_info()
