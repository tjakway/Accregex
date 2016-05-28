from .eprint import eprint

import os
import operator
from gnucash import Session, GncNumeric, Split
from datetime import datetime, date
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
#will throw an exception if any account in account_rules does not exist
def check_accounts_exist(root_account, account_rules):
    #exception formatting helper function
    def mk_account_exception(rule, which_account):
        AccountNotFoundException("Invalid {} account found in rule: {} for rule {}" \
                    .format(which_account, getattr(rule, which_account), str(rule)))

    def account_exists(rule, which_account):
        account = root_account.lookup_by_name(getattr(rule, which_account))
        return account is None

    #check every account in every rule (i.e. 2 accounts per rule)
    for this_rule in account_rules:
        for which_account in ["dest", "src"]:
            #check for null accounts
            if getattr(this_rule, which_account) is None:
                raise mk_account_exception(this_rule, which_account)
            #make sure the account actually exists
            if not account_exists(this_rule, which_account):
                raise mk_account_exception(this_rule, which_account)

#get the set of all source accounts in account_rules
def get_source_account_set(root_account, account_rules):
    accounts = set()

    for this_rule in account_rules:
        src_account = root_account.lookup_by_name(this_rule.src)
        accounts.add(src_account)

    return accounts

#return splits based on the passed comparison function (INCLUSIVE)
def _splits_comp_date(split_list, p_date, comparator):
    matching_splits = []
    for this_split in split_list:
        trans = this_split.parent
        trans_date = date.fromtimestamp(trans.GetDate())

        if comparator(trans_date, p_date):
            matching_splits.append(this_split)
 
    return matching_splits

def splits_after_date(split_list, p_date):
    return _splits_comp_date(split_list, p_date, operator.ge)

def splits_before_date(split_list, p_date):
    return _splits_comp_date(split_list, p_date, operator.le)

#return only splits for which the comparator only its amount returns True
def _splits_filter_amount(split_list, comparator):
    matching_splits = []
    for this_split in split_list:
        split_amount = gnc_numeric_to_python_Decimal(split.GetAmount())        

        if comparator(split_amount):
            matching_splits.append(this_split)

    return matching_splits

ZERO = Decimal(0)

def splits_filter_debits(split_list):
    return _splits_filter_amount(split_list, lambda x: x < ZERO)

def splits_filter_credits(split_list):
    return _splits_filter_amount(split_list, lambda x: x > ZERO)

def run(input_file, account_rules):
    session = sessionForFile(input_file)
    root_account = session.book.get_root_account()

    #make sure all accounts exist before running any rules
    check_accounts_exist(root_account, account_rules) 


