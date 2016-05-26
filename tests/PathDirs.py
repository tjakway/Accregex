import sys
import os

#add the parent directory of this script's location to PYTHONPATH (*NOT* the parent dir of the current working directory!)
def add_parent_to_path():
    parent_dir = os.path.abspath("..")

    #add the parent directory to PYTHONPATH
    #see http://stackoverflow.com/questions/4934806/how-can-i-find-scripts-directory-with-python
    sys.path.insert(0, parent_dir)

    all_child_dirs = []
    for d in os.listdir(parent_dir):
        #ignore hidden directories
        if not d.startswith("."):
            all_child_dirs = [os.path.join(parent_dir, d)] + all_child_dirs


    #add all of its child directories to PYTHONPATH
    for f in all_child_dirs:
        if os.path.isdir(f):
            sys.path.insert(0, f)
