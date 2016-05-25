import sys
import os

#add the parent directory of this script's location to PYTHONPATH (*NOT* the parent dir of the current working directory!)
def add_parent_to_path():
    #get the parent directory of wherever this script is located
    #(i.e. the script location, not the working directory)
    #see http://stackoverflow.com/questions/30218802/get-parent-of-current-directory-from-python-script
    parent_dir = os.path.dirname(sys.path[0])
    if parent_dir == '' or parent_dir is None:
        parent_dir = os.getcwd()

    #add the parent directory to PYTHONPATH
    #see http://stackoverflow.com/questions/4934806/how-can-i-find-scripts-directory-with-python
    sys.path.insert(0, parent_dir)

    #add all of its child directories to PYTHONPATH
    for f in os.listdir(parent_dir):
        if os.path.isdir(f):
            sys.path.insert(0, f)
