#!/usr/bin/env python2

from __future__ import print_function
import sys
import csv
import shutil
import os
from gnucash import Session, GncNumeric, Split
from .Logger import Logger
from .Args import get_cli_arg_parser

#create the global logger
global_logger = Logger()



#copy the input file and return the name of the destination file
def copy_input(input_file):
   new_file = input_file + ".bak"
   shutil.copy(input_file, new_file)
   return new_file


def get_account(root, acc_name):
    root.lookup_by_name(acc_name)


def sessionForFile(input_file):
    try:
        return Session(os.path.abspath(input_file))
    except GnuCashBackendException, backend_exception:
        if ERR_BACKEND_LOCKED in backend_exception.errors:
            eprint("Cannot open %s, file is locked." % input_file)
        raise
    
    
def accregex_main(argv=None):
    if argv == None:
        argv = sys.argv

    #parse cli args
    parser = get_cli_arg_parser()
    

    try:
        #print help and exit if no arguments were passed
        if not len(argv) > 1:
            raise "No arguments passed!"

        args = parser.parse_args(argv)
    except:
        parser.print_help()
        sys.exit(0)
    
    #done with argument parsing

    #make file backup if necessary
    if not args.inplace:
        res_file = copy_input(args.file)
        global_logger.write("Copied gnucash input file: {} to {}".format(args.file, res_file))

    
    
