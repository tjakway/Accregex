from AccregexTest import AccregexTest

#just use UUID's--simple and effective
def rand_string():
    import uuid; 
    return str(uuid.uuid4()).upper()

def get_account_fully_qualified_name(account, name = ""):
    curr_depth = account.get_current_depth()
    new_name = account.GetName() + ":" + name
    if curr_depth > 0:
        return get_account_fully_qualified_name(account.get_parent(), new_name)
    else:
        return new_name

class TestReadAccountData(AccregexTest):
    asset_base = "Assets:Current Assets:"
    asset_accounts = [asset_base + "Checking Account",
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
        #we're not making any changes--don't need to commit
        self.session.end()

    def testRootDepth(self):
        self.assertEqual(self.root.get_current_depth(), 0)

    def assertRecursiveFindAccounts(self, root_account, accounts):
        from accregex import Account
        for a in accounts:
           this_account = Account.get_account(root_account, a)
           #make sure we found the right account
           get_account_fully_qualified_name(this_account) == a

    def testFindAssetAccounts(self):
        self.assertRecursiveFindAccounts(self.root, self.asset_accounts)

    def testFindExpenseAccounts(self):
        self.assertRecursiveFindAccounts(self.root, self.expense_accounts)

    #pass a function that returns the account name
    def assertAccountDoesntExist(self, get_account_name):
        account_name = get_account_name()
        from accregex import Account
        self.assertEqual(Account.get_account(self.root, account_name), None)
        
    #just use a basic random string
    def testFindNonexistantRandomAccount(self):
        self.assertAccountDoesntExist(rand_string)

    #more devious version.  generate several random strings concatenated with ":"
    def testFindNonexistantRandomHierarchicalAccount(self):
        def rand_hierarchical_string():
            #random is seeded on first import
            ret_str = ""
            import random
            for i in range(1, random.randrange(2, 10)):
                ret_str = ret_str + ":" + rand_string()
            return ret_str

        self.assertAccountDoesntExist(rand_hierarchical_string)

