import unittest

"""
The base class for tests
The purpose of this module is to add the source directory to PYTHONPATH
"""

class AccregexTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        import unittest
        #need to mess with PYTHONPATH to import from a relative location
        import PathDirs
        PathDirs.add_parent_to_path()

    @classmethod
    def tearDownClass(cls):
        pass
    
    #resource paths
    match_all_unspecified_json = "res/match_all_unspecified.json"
    reg_doc_example = "res/reg_doc_example.gnucash"
    parking_fee_rule_json = "res/parking_fee_rule.json"
