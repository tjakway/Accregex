import shlex
import os
from decimal import Decimal
from AccregexTest import AccregexTest
from PathDirs import get_parent_of_cwd

parking_expense_account_full_name = "Expenses:Auto:Parking"

class TestChangeParking(AccregexTest):
    def setUp(self):
        AccregexTest.setUp(self)

    def tearDown(self):
        AccregexTest.tearDown(self)

    #launch main with the necessary parameters
    def instrument_main(self):
        paths = map(os.path.abspath, [AccregexTest.reg_doc_example, AccregexTest.parking_fee_rule_json])
        extra_argv = shlex.split("-f {} -r {} -s '2000-05-01' --inplace -v" \
                .format(*paths))
        from accregex import Env

        prev_cwd = os.getcwd()
        #if we don't change the cwd we'll try to run the program from accregex/tests/ and it won't find the module
        #run from the directory above where this file is defined
        from .PathDirs import abs_this_dir, parent_of
        run_from = parent_of(abs_this_dir())
        os.chdir(run_from)
        Env.choose_main(extra_argv)

        #cd back to our original directory
        os.chdir(prev_cwd)

    def runTest(self):
        self.instrument_main()

        try:
            from accregex import Account
            from accregex.AccountUtil import gnc_numeric_to_python_Decimal
            from gnucash import Session
            session = Session(AccregexTest.reg_doc_example, is_new=False, ignore_lock=False)
            parking_expense_account = Account.get_account(session.book.get_root_account(), parking_expense_account_full_name)
            actual_balance = gnc_numeric_to_python_Decimal(parking_expense_account.GetBalance())
            expected_balance = Decimal(25)

            self.assertEqual(actual_balance, expected_balance)
            
            #TODO: check that the actual transaction in Assets:Current Assets:Checking Account was changed
            session.end()
        except:
            #in case of error close the session and re-raise
            #equivalent to a "finally" block
            if "session" in locals():
                session.end()
            raise



