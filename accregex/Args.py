import argparse

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
    parser.add_argument('--no-relaunch', dest='norelaunch', help="Don't relaunch with gnucash-env (should not be passed except for debugging)", action="store_true")
    return parser

def need_relaunch(argv):
    return get_cli_arg_parser().parse_args(argv).norelaunch == False
