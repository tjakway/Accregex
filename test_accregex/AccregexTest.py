import unittest
from .PathDirs import abs_from_here, add_accregex_to_python_path

"""
The base class for tests
The purpose of this module is to add the source directory to PYTHONPATH
"""

class AccregexTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        import unittest
        #need to mess with PYTHONPATH to import from a relative location
        add_accregex_to_python_path()

    @classmethod
    def tearDownClass(cls):
        pass
    
    #resource paths
    match_all_unspecified_json = abs_from_here("res/match_all_unspecified.json")
    reg_doc_example = abs_from_here("res/reg_doc_example.gnucash")
    parking_fee_rule_json = abs_from_here("res/parking_fee_rule.json")
