from .eprint import eprint

import os
from gnucash import Session, GncNumeric, Split
from accregex.AccountRule import AccountNotFoundException

def get_account(root, acc_name):
    root.lookup_by_name(acc_name)

def sessionForFile(input_file):
    try:
        return Session(os.path.abspath(input_file))
    except GnuCashBackendException, backend_exception:
        if ERR_BACKEND_LOCKED in backend_exception.errors:
            eprint("Cannot open %s, file is locked." % input_file)
        raise

#check that all dest and source accounts pointed to by the list of AccountRules are real
def check_accounts_exist(account_rules):
    #exception formatting helper function
    def mk_account_exception(rule, which_account):
        AccountNotFoundException("Invalid {} account found in rule: {} for rule {}" \
                    .format(which_account, getattr(rule, which_account), str(rule)))

    for this_rule in account_rules:
        #check for null accounts
        if this_rule.dest is None:
            raise mk_account_exception(this_rule, "dest")
        if this_rule.src is None:
            raise mk_account_exception(this_rule, "src")

        
        


def run(input_file, account_rules):
    session = sessionForFile(input_file)
    root_account = gnucash_session.book.get_root_account()
    account_of_interest = account_from_path(root_account, account_path)

