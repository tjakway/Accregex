from AccregexTest import AccregexTest

#just use UUID's--simple and effective
def rand_string():
    import uuid; 
    return str(uuid.uuid4()).upper()

class TestReadAccountData(AccregexTest):
    asset_base = "Assets:Current Assets:"
    checking_account = asset_base + "Checking Account"
    asset_accounts = [checking_account,
            asset_base + "Savings Account",
            asset_base + "Cash in Wallet"]
    expense_base = "Expenses:Auto:"
    expense_accounts = [expense_base + "Gas",
            expense_base + "Repair and Maintenance"]

    def setUp(self):
        AccregexTest.setUp(self)
        from accregex import Account
        self.session = Account.sessionForFile(AccregexTest.reg_doc_example)
        self.root = self.session.book.get_root_account()

    def tearDown(self):
        AccregexTest.tearDown(self)
        #we're not making any changes--don't need to commit
        self.session.end()

    def assertRecursiveFindAccounts(self, root_account, accounts):
        from accregex import Account
        for a in accounts:
           this_account = Account.get_account(root_account, a)
           #make sure we found the right account
           Account.get_account_fully_qualified_name(this_account) == a

    #pass a function that returns the account name
    def assertAccountDoesntExist(self, get_account_name):
        account_name = get_account_name()
        from accregex import Account
        self.assertIs(Account.get_account(self.root, account_name), None)

class TestRootDepth(TestReadAccountData):
    def runTest(self):
        self.assertEqual(self.root.get_current_depth(), 0)

class TestFindAssetAccounts(TestReadAccountData):
    def runTest(self):
        self.assertRecursiveFindAccounts(self.root, self.asset_accounts)

class TestFindExpenseAccounts(TestReadAccountData):
    def runTest(self):
        self.assertRecursiveFindAccounts(self.root, self.expense_accounts)

class TestFindNonexistantRandomAccount(TestReadAccountData):
    #just use a basic random string
    def runTest(self):
        self.assertAccountDoesntExist(rand_string)

class TestFindNonexistantRandomHierarchicalAccount(TestReadAccountData):
    #more devious version.  generate several random strings concatenated with ":"
    def runTest(self):
        def rand_hierarchical_string():
            #random is seeded on first import
            ret_str = ""
            import random
            for i in range(1, random.randrange(2, 10)):
                ret_str = ret_str + ":" + rand_string()
            return ret_str

        self.assertAccountDoesntExist(rand_hierarchical_string)
