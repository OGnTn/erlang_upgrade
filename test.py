import subprocess
import shlex
import time

miauw = shlex.split("erl +S 4 -noshell -s benchmark experiment_setup -s init stop")
print(miauw)

start = time.time()
subprocess.run(miauw, shell=True)
end = time.time()
print(end - start)

#print("The exit code was: %d" % list_files.returncode)