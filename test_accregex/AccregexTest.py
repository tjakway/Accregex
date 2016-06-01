import unittest

"""
The base class for tests
The purpose of this module is to add the source directory to PYTHONPATH
"""


#get the absolute path of a file relative to 
#see http://stackoverflow.com/questions/5137497/find-current-directory-and-files-directory
def abs_from_here(path):
    import os
    return os.path.join(os.path.dirname(os.path.realpath(__file__)), path)

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
    match_all_unspecified_json = abs_from_here("res/match_all_unspecified.json")
    reg_doc_example = abs_from_here("res/reg_doc_example.gnucash")
    parking_fee_rule_json = abs_from_here("res/parking_fee_rule.json")
