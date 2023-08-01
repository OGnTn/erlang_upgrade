import subprocess
import shlex
import time

miauw = shlex.split("erl +S 4 -noshell -s benchmark test_bench -s init stop")
print(miauw)
file = open("test.txt", "w")

start = time.time()
#result = subprocess.run(miauw, stdout=file, text=True, bufsize=1)
process = subprocess.check_output(miauw, universal_newlines=True)
print(process)

end = time.time()
print(end - start)

print("miauw")

#print("The exit code was: %d" % list_files.returncode)