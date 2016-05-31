import argparse
import datetime

#see http://stackoverflow.com/questions/25470844/specify-format-for-input-arguments-argparse-python
def _valid_date(s):
    try:
        return datetime.datetime.strptime(s, "%Y-%m-%d")
    except ValueError:
        msg = "Not a valid date: '{0}'.".format(s)
        raise argparse.ArgumentTypeError(msg)

def get_cli_arg_parser():
    parser = argparse.ArgumentParser()
    
    #can't pass -q and -v at the same time
    group = parser.add_mutually_exclusive_group()
    group.add_argument('-q', '--quiet', dest='quiet', help='Suppress output', action="store_true")
    group.add_argument('-v', '--verbose', dest='verbose', help='Verbose output', action="store_true")

    parser.add_argument('-f', '--input-file', dest='file', required=True, help='Gnucash input file')
    parser.add_argument('-r', '--rule-file', dest='rulefile', required=True, help='JSON rule file')
    parser.add_argument('--inplace', dest='inplace', help='Don\'t create a backup of the Gnucash file', action="store_true")
    parser.add_argument('--no-relaunch', dest='norelaunch', help="Don't relaunch with gnucash-env (should not be passed except for debugging)", action="store_true")

    #date range
    #start date is required, end date is optional
    #see http://stackoverflow.com/questions/25470844/specify-format-for-input-arguments-argparse-python
    parser.add_argument('-s', "--startdate", dest="startdate", help="The Start Date - format YYYY-MM-DD ", required=True, type=_valid_date)
    parser.add_argument('-e', "--enddate", dest="enddate", help="The End Date - format YYYY-MM-DD  (If no end date is specified, assumed to mean start date -> last transaction)", required=False, type=_valid_date)

    return parser

def need_relaunch(argv):
    return get_cli_arg_parser().parse_args(argv).norelaunch == False

def verbose_enabled(argv):
    if argv is [] or argv is None:
        return False
    else:
        return get_cli_arg_parser().parse_args(argv).verbose == True
