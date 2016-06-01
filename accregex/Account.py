from .eprint import eprint

import os
import operator
from gnucash import Session, GncNumeric, Split, GnuCashBackendException, ERR_BACKEND_LOCKED
from datetime import datetime, date
from accregex.AccountRule import AccountNotFoundException, get_most_urgent_priority_rule
from decimal import Decimal
from AccountUtil import gnc_numeric_to_python_Decimal


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
    #lookup_by_name expects a python string object, not a unicode object
    next_account = top_account.lookup_by_name(str(query_top_account))
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
    #skip the root account (curr_depth = 0)
    if curr_depth > 1:
        return get_account_fully_qualified_name(account.get_parent(), new_name)
    else:
        #remove the last colon
        return new_name[:-1]

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
        return AccountNotFoundException("Invalid {} account found in rule: {} for rule {}" \
                    .format(which_account, getattr(rule, which_account), str(rule)))

    def account_exists(rule, which_account):
        account = get_account(root_account, getattr(rule, which_account))
        return account is not None and \
            get_account_fully_qualified_name(account) == getattr(rule, which_account)

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
        src_account = get_account(root_account, this_rule.src)
        if src_account is None:
            eprint("Could not find account {}".format(this_rule.src))
            return None
        accounts.add(src_account)

    if accounts != []:
        return accounts
    else:
        return None

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
        split_amount = gnc_numeric_to_python_Decimal(this_split.GetAmount())

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

def copy_split(a): 
    from .Reflect import chain_mutations
    b = Split(a.GetBook())
    #hack to copy the data over since Split doesn't have Clone()
    chain_mutations(b, a)
    return b

def move_split(root_account, split, rule):
    try:
        parent_transaction = split.GetParent()
        parent_transaction.BeginEdit()
        txn_splits = parent_transaction.GetSplitList()
        
        #get the debit ("dest") splits
        debit_splits = splits_filter_debits(txn_splits)
        assert debit_splits != []
        assert debit_splits is not None
        #get undefined splits
        undef_splits = get_undefined_splits(debit_splits) 

        #if we're calling move_split it means we already found what we want to move--
        #this can only be a bug
        assert undef_splits != []
        assert undef_splits is not None

        #lookup the accounts for this rule
        src_account = get_account(root_account, rule.src)
        new_dest_account = get_account(root_account, rule.dest)

        #make sure we're referring to the right source account
        assert src_account.name == split.GetAccount().name
        for this_undef_split in undef_splits:
            try:
                src_account.BeginEdit()
                new_dest_account.BeginEdit()
                old_dest_account = this_undef_split.GetAccount()
                old_dest_account.BeginEdit()

                src_split = this_undef_split.GetOtherSplit()

                this_undef_split.SetAccount(new_dest_account)
                this_undef_split.SetValue(src_split.GetValue().neg())
                this_undef_split.SetAmount(src_split.GetAmount().neg())

                src_account.CommitEdit()
                new_dest_account.CommitEdit()
                old_dest_account.CommitEdit()
            except:
               #idiotically, Account has "BeginEdit" and "CommitEdit" but no
               #Delete/Rollback/Undo/Abort Edit or anything that would
               #serve that purpose
               # if "src_account" in locals():
               #     src_account.RollbackEdit()
                raise
        
        parent_transaction.CommitEdit()
    except:
        if "parent_transaction" in locals():
            parent_transaction.RollbackEdit()
        raise

#get only (debit) splits where the destination account is Undefined
def get_undefined_splits(splits):
    undefined_splits = []
    for i in splits:
        other_split = i.GetOtherSplit()
        if other_split is not None and other_split.GetAccount().name == "Undefined":
            undefined_splits.append(i)

    return undefined_splits
    

def process_source_account(src_acc, account_rules, start_date, end_date=None):
    splits = src_acc.GetSplitList()

    #the filters to apply:
    #1. date range
    #2. debits
    #3. Undefined accounts

    if end_date is not None:
        splits = splits_before_date(splits, end_date)

    splits = get_undefined_splits(splits_filter_debits(splits))

    for this_split in splits:
        matching_rules = get_matching_rules(this_split.GetParent().GetDescription(), account_rules)
        if matching_rules is not []:
            urgent_priority_rule = get_most_urgent_priority_rule(matching_rules)
            assert urgent_priority_rule is not None
            move_split(src_acc.get_root(), this_split, urgent_priority_rule)


def run(input_file, account_rules, start_date, end_date=None):
    try:
        session = sessionForFile(input_file)
        root_account = session.book.get_root_account()

        #make sure all accounts exist before running any rules
        check_accounts_exist(root_account, account_rules) 

        source_account_set = get_source_account_set(root_account, account_rules)
        if source_account_set is not None:
            assert source_account_set != []
            for src_acc in source_account_set:
                process_source_account(src_acc, account_rules, start_date, end_date)
            #only save if we've made changes
            session.save()
            if session.book.session_not_saved():
                raise "Session book was not saved!"


        session.end()
    except:
        if "session" in locals():
            session.end()
        raise
