#!/usr/bin/env python

import sys
import argparse
import csv
import shutil
from gnucash import Session, GncNumeric, Split

#this logging class doesn't actually print anything
#it just stores a string (thus no side effects)
class Logger:
    _log_string = ""
    
    def write(self, write_str):
        self._log_string += str(write_str)
        self._log_string += '\n'

    def get_log(self):
        return _log_string

#create the global logger
global_logger = Logger()

def get_cli_arg_parser():
    parser = argparse.ArgumentParser()
    
    #can't pass -q and -v at the same time
    group = parser.add_mutually_exclusive_group()
    group.add_argument('-q', '--quiet', dest='quiet', help='Suppress output', action="store_true")
    group.add_argument('-v', '--verbose', dest='verbose', help='Verbose output', action="store_true")

    parser.add_argument('-f', '--input-file', dest='file', help='Gnucash input file')
    #todo: list accepted file formats
    parser.add_argument('-b', '--bank-file', dest='bankfile', help='Transactions to import')
    parser.add_argument('--inplace', dest='inplace', help='Don\'t create a backup of the Gnucash file', action="store_true")
    return parser

#copy the input file and return the name of the destination file
def copy_input(input_file):
   new_file = input_file + ".bak"
   shutil.copy(input_file, new_file)
   return new_file


def get_account(root, acc_name):
    root.lookup_by_name(acc_name)


#def sessionWithFile(input_file):
    
def main():
    #parse cli args
    parser = get_cli_arg_parser()
    try:
        args = parser.parse_args()
    except:
        parser.print_help()
        sys.exit(0)
    
    if not args.inplace:
        res_file = copy_input(args.file)
        global_logger.write("Copied {} to {}".format(args.file, res_file))

    
