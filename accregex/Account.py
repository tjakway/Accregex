from .eprint import eprint

import os
from gnucash import Session, GncNumeric, Split

def get_account(root, acc_name):
    root.lookup_by_name(acc_name)


def sessionForFile(input_file):
    try:
        return Session(os.path.abspath(input_file))
    except GnuCashBackendException, backend_exception:
        if ERR_BACKEND_LOCKED in backend_exception.errors:
            eprint("Cannot open %s, file is locked." % input_file)
        raise

