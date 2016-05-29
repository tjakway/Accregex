import unittest
#need to mess with PYTHONPATH to import from a relative location
import PathDirs
PathDirs.add_parent_to_path()
from accregex import AccountRule



class TestReadSimpleJSON(unittest.TestCase):
    def test_simple_json(self):
        #list of account rules
        account_rules = AccountRule.read_account_rules("res/match_all_unspecified.json")
        self.assertEqual(len(account_rules), 1)
