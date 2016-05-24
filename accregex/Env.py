import sys
import os
from eprint import eprint
from Args import need_relaunch
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
    os.getcwd()
    
    accregex_proc = Popen(argv, executable=gnucash_env,  )

def relauncher_main(argv=None):
    if argv == None:
        argv = sys.argv

    additional_args = ["python2", "-maccregex"]

    #find gnucash-env, fail if we can't find it
    gnucash_env = find_prog("gnucash-env")
    if gnucash_env == None:
        eprint("Could not find gnucash-env!  Is GnuCash correctly installed?")
        sys.exit(1)

    

#this is where program execution actually starts
#if --no-relaunch was passed, run Accregex.accregex_main
#otherwise, run Env.relauncher_main
def choose_main(argv=None):
    if argv == None:
        argv = sys.argv

   if need_relaunch(argv):
       relauncher_main(argv)
   else:
       accregex_main(argv)
