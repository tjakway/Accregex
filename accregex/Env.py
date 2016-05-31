from __future__ import print_function
import sys
import os
import shlex
from subprocess import Popen
from eprint import eprint
from Args import need_relaunch, verbose_enabled
from Accregex import accregex_main

#search environment for executable
#see http://stackoverflow.com/questions/377017/test-if-executable-exists-in-python
#and the comment by http://stackoverflow.com/users/115497/kevin-ivarsen
def find_prog(program):
    #correctly handle exe extension in windows
    if sys.platform == "win32" and not program.endswith(".exe"):
        program += ".exe"

    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None

def relaunch(gnucash_env, argv):
    ccwd = os.getcwd()

    gnucash_env_path = os.path.abspath(gnucash_env)
    #Popen expects the program path to be the first item in argv if you pass a sequence
    accregex_proc = Popen([gnucash_env_path] + argv, bufsize=-1, executable=gnucash_env, cwd=ccwd)
    accregex_proc.wait()

def relauncher_main(argv=None):
    if argv == None:
        argv = sys.argv

    #find gnucash-env, fail if we can't find it
    gnucash_env = find_prog("gnucash-env")
    if gnucash_env == None:
        eprint("Could not find gnucash-env!  Is GnuCash correctly installed?")
        sys.exit(1)

    #need to pass the --no-relaunch flag to make sure we don't get stuck in an infinite loop
    #this will make choose_main run accregex_main instead of relauncher_main
    additional_args = shlex.split("python2 -maccregex --no-relaunch")
    new_argv = additional_args + argv

    relaunch(gnucash_env, new_argv)
    

#this is where program execution actually starts
#if --no-relaunch was passed, run Accregex.accregex_main
#otherwise, run Env.relauncher_main
def choose_main(argv=None):
    if argv == None:
        argv = sys.argv

    if(verbose_enabled(argv)):
        print("Env.choose_main called with {}".format(str(argv)))

    #remove the program name from arg parsing
    if len(argv) > 0 and "__main__.py" in argv[0]:
        del argv[0]

    #if no args are passed we need to relaunch
    if len(argv) == 0 or need_relaunch(argv):
        relauncher_main(argv)
    else:
        accregex_main(argv)
