import json
import re
from .eprint import eprint

class AccountRule:
    def __init__(self, rule_name, regex_str, priority, dest, src):
       self.rule_name = rule_name
       self.priority = priority
       self.regex = re.compile(regex_str)
       #the destination account
       self.dest = dest
       self.src = src

#construct an AccountRule object from a 2nd-tier JSON object
def _json_obj_to_account_rule(rule_name, global_src_account, obj):
    #either use the global source account or the source account specified for this rule
    #prefer the per-rule source account if both exist
    #if neither exist it's an error
    try:
        if global_src_account is None:
            try:
                src_acc = obj['src']
            except:
                eprint("Must specify either a default (global) source account with a top-level 'src' element or a per-rule source account.")
                raise
        else:
            src_acc = global_src_account

        return AccountRule(rule_name, \
                obj['regex'], \
                obj['priority'], \
                obj['dest'], \
                src_acc)
    except re.error:
        eprint("error compiling regex expression in AccountRule from json object {}".format(str(obj)))
        raise

#read account rules from a JSON file
#next need to make sure the named destination accounts correspond to actual accounts
def read_account_rules(json_file_name):
    with open(json_file_name) as json_file:
        json_data = json.load(json_file)

    all_rules = []

    #can optionally specify a "src" element as the default source account
    #can be overridden on a per-rule basis
    if "src" in json_data.keys():
        default_src = json_data["src"]
    else:
        default_src = None

    #read in each account rule from each JSON sub-object 
    #the top-level keys correspond to each rule
    for key in json_data.keys():
        #ignore the 'src' element--it isn't a rule
        if key is "src":
            pass

        rule_name = key
        
        try:
            all_rules.append(_json_obj_to_account_rule(rule_name, default_src, json_data[rule_name]))
        except:
            eprint("Could not create a rule from JSON object {}".format(rule_name))
            raise

    return all_rules 
