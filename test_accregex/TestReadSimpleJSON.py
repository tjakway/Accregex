from AccregexTest import AccregexTest

class TestReadSimpleJSON(AccregexTest):
    def setUp(self):
        AccregexTest.setUp(self)

    def runTest(self):
        from accregex import AccountRule
        #list of account rules
        account_rules = AccountRule.read_account_rules(AccregexTest.match_all_unspecified_json)
        self.assertEqual(len(account_rules), 1)
