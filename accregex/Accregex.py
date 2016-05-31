#!/usr/bin/env python2

import sys
import csv
import shutil
import os
from gnucash import Session, GncNumeric, Split
from .Logger import Logger
from .Args import get_cli_arg_parser
from .Account import run
from .AccountRule import read_account_rules

#create the global logger
global_logger = Logger()



#copy the input file and return the name of the destination file
def copy_input(input_file):
   new_file = input_file + ".bak"
   shutil.copy(input_file, new_file)
   return new_file

def accregex_main(argv=None):
    if argv == None:
        argv = sys.argv

    #parse cli args
    parser = get_cli_arg_parser()
    

    try:
        #print help and exit if no arguments were passed
        if not len(argv) > 1:
            raise InputError("No arguments passed!")

        args = parser.parse_args(argv)
    except:
        parser.print_help()
        sys.exit(0)
    
    #done with argument parsing

    #make file backup if necessary
    if not args.inplace:
        res_file = copy_input(args.file)
        global_logger.write("Copied gnucash input file: {} to {}".format(args.file, res_file))

    #read in account rules
    account_rules = read_account_rules(args.rulefile)
   
    #enddate argument is optional
    try:
        enddate = args.enddate
    except:
        enddate = None

    #do the actual work
    run(args.file, account_rules, args.startdate, enddate)
