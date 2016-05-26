import unittest
#need to mess with PYTHONPATH to import from a relative location
import PathDirs
PathDirs.add_parent_to_path()
import accregex.AccountRule



class ReadSimpleJSONTests(unittest.TestCase):
    def test_simple_json(self):
        self.assertEqual(len(AccountRule.read_account_rules("match_all_unspecified.json")), 1)
