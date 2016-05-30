from .eprint import eprint

import os
import operator
from gnucash import Session, GncNumeric, Split, GnuCashBackendException, ERR_BACKEND_LOCKED
from datetime import datetime, date
from accregex.AccountRule import AccountNotFoundException
from decimal import Decimal


#get the account from a colon separated account hierarchy
#e.g. "Expenses:Auto:Gas"
def get_account(top_account, acc_name):
    if top_account is None or acc_name is None:
        return None

    s = acc_name.split(":", 1)
    query_top_account = s[0]
    if len(s) > 1:
        query_next_accounts = s[1]
    else:
        query_next_accounts = s[0]

    #lookup_by_name only does a depth-1 search
    next_account = top_account.lookup_by_name(query_top_account)
    if not ":" in acc_name:
        #if there are no more colons, we're at the end of the list
        #return it if we've found it
        return next_account
    else:
        #walk down the hierarchy
        return get_account(next_account, query_next_accounts)

def get_account_fully_qualified_name(account, name = ""):
    curr_depth = account.get_current_depth()
    new_name = account.GetName() + ":" + name
    if curr_depth > 0:
        return get_account_fully_qualified_name(account.get_parent(), new_name)
    else:
        return new_name

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
        account = get_account(root_account, getattr(rule, which_account))
        return account is not None

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

#don't forget to handle splits that no rules match
def get_matching_rules(description, rules):
    matching_rules = []
    for this_rule in rules:
        if this_rule.regex.match(description) is not None:
            matching_rules.append(this_rule)

    return matching_rules

def get_highest_priority_rule(rules):
    return sorted(rules, key=AccountRule.priority)[-1:]

def move_split(split, rule):
    try:
        parent_transaction = split.GetParent()
        parent_transaction.BeginEditing()
        txn_splits = parent_transaction.GetSplitList()

        #get the debit ("dest") splits
        debit_splits = splits_filter_debits(txn_splits)

        #lookup the account for this rule
        new_account = get_account(rule)
        for i in debit_splits:
            debit_splits.SetAccount(new_account)
        
        parent_transaction.CommitEdit() 
    except:
        if "parent_transaction" in locals():
            parent_transaction.RollbackEdit()
        raise

#get only (debit) splits where the destination account is Undefined
def filter_undefined_splits(account):
    debit_splits = splits_filter_debits(account.GetSplitList())
    undefined_splits = []
    for i in debit_splits:
        if i.GetAccount().name is "Undefined":
            undefined_splits.append(i)

    return undefined_splits
    

def run(input_file, account_rules):
    session = sessionForFile(input_file)
    root_account = session.book.get_root_account()

    #make sure all accounts exist before running any rules
    check_accounts_exist(root_account, account_rules) 

     

