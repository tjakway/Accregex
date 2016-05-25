import json
from .eprint import eprint

class AccountRule:
    def __init__(self, rule_name, regex_str, priority, dest):
       self.rule_name = rule_name
       self.priority = priority
       self.regex = re.compile(regex_str)
       #the destination account
       self.dest = dest

#construct an AccountRule object from a 2nd-tier JSON object
def _json_obj_to_account_rule(rule_name, obj):
    try:
        return AccountRule(rule_name, \
                obj['regex'], \
                obj['priority'], \
                obj['dest'])
    except re.error:
        eprint("error compiling regex expression in AccountRule from json object {}".format(str(obj)))
        raise

#read account rules from a JSON file
#next need to make sure the named destination accounts correspond to actual accounts
def read_account_rules(json_file):
    json_data = json.load(json_file)
    all_rules = []

    #read in each account rule from each JSON sub-object 
    #the top-level keys correspond to each rule
    for key in json_data.keys():
        rule_name = key
        
        try:
            all_rules.append(_json_obj_to_account_rule(rule_name, json_data[rule_name]))
        except:
            eprint("Could not create a rule from JSON object {}".format(rule_name))
            raise

    return all_rules 
