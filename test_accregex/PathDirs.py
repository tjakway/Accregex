import sys
import os
import os.path

def abs_this_dir():
    return os.path.abspath(os.path.dirname(os.path.realpath(__file__)))

#get the directory this python file is defined in
#see http://stackoverflow.com/questions/5137497/find-current-directory-and-files-directory
def abs_from_here(path):
    return os.path.join(abs_this_dir(), path)

#check if pypathdir is already in PYTHONPATH and add it if it isn't
def add_path_no_dup(pypathdir):
    if not pypathdir in sys.path:
        sys.path.insert(0, pypathdir)

def get_parent_of_cwd():
    return os.path.abspath("..")

def all_child_dirs(path):
    child_dirs = []
    for d in os.listdir(path):
        #ignore hidden directories
        if not d.startswith("."):
            child_dirs = [os.path.join(path, d)] + child_dirs

    return child_dirs

def add_child_dirs_to_path(path):
    child_dirs = all_child_dirs(path)

    #add all of its child directories to PYTHONPATH
    for f in child_dirs:
        if os.path.isdir(f):
            add_path_no_dup(f)

#add the parent directory of this script's location to PYTHONPATH (*NOT* the parent dir of the current working directory!)
def _add_parent_of_cwd_to_path():
    parent_dir = get_parent_of_cwd()

    #add the parent directory to PYTHONPATH
    #see http://stackoverflow.com/questions/4934806/how-can-i-find-scripts-directory-with-python
    add_path_no_dup(parent_dir)
    add_child_dirs_to_path(parent_dir)

def parent_of(path):
    #see http://stackoverflow.com/questions/2860153/how-do-i-get-the-parent-directory-in-python
    return os.path.abspath(os.path.join(path, os.pardir))

def get_accregex_abs_path():
    return os.path.join(parent_of(abs_this_dir()), "accregex")

def add_accregex_to_python_path():
    #add .. and ../accregex/ to PYTHONPATH
    abs_parent = parent_of(abs_this_dir())
    add_child_dirs_to_path(abs_parent)
    add_path_no_dup(get_accregex_abs_path())
    add_path_no_dup(abs_parent)
    print str(sys.path)
