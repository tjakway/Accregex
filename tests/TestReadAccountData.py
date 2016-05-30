from AccregexTest import AccregexTest

def get_account_fully_qualified_name(account, name = []):
    curr_depth = account.get_current_depth()
    new_name = account.GetName() + [":"] + name
    if curr_depth > 0:
        return get_account_fully_qualified_name(account.get_parent(), new_name)
    else:
        return new_name

class TestReadAccountData(AccregexTest):
    asset_base = "Assets:Current Assets:"
    asset_accounts = [asset_base + "Checking Account",
            asset_base + "Savings Account",
            asset_base + "Cash in Wallet"]
    expense_base = "Expenses:Auto"
    expense_accounts = [expense_base + "Gas",
            expense_base + "Repair and Maintenance"]

    def setUp(self):
        AccregexTest.setUp(self)
        from accregex import Account
        self.session = Account.sessionForFile(AccregexTest.reg_doc_example)
        self.root = session.book.get_root_account()

    def tearDown(self):
        #we're not making any changes--don't need to commit
        self.session.close()

    def testRootDepth(self):
        assertEqual(self.root.get_current_depth() == 0)

    def assertRecursiveFindAccounts(self, root_account, accounts):
        for a in accounts:
           this_account = Account.get_account(root_account, a)
           #make sure we found the right account
           get_account_fully_qualified_name(this_account) == a
