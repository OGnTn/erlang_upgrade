import subprocess
import shlex

miauw = shlex.split("erl +S 4 -noshell -s benchmark test_test_para_inst -s init stop")
print(miauw)

subprocess.run(miauw, shell=True)
#print("The exit code was: %d" % list_files.returncode)