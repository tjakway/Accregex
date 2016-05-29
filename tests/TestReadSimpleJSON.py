from AccregexTest import AccregexTest



class TestReadSimpleJSON(AccregexTest):
    def test_simple_json(self):
        
        from accregex import AccountRule
        #list of account rules
        account_rules = AccountRule.read_account_rules("res/match_all_unspecified.json")
        self.assertEqual(len(account_rules), 1)
