import shlex
from decimal import Decimal
from AccregexTest import AccregexTest

parking_expense_account_full_name = "Expenses:Auto:Parking"

class TestChangeParking(AccregexTest):
    def setUp(self):
        AccregexTest.setUp(self)

    def tearDown(self):
        AccregexTest.tearDown(self)

    #launch main with the necessary parameters
    def instrument_main(self):
        extra_argv = shlex.split("-f {} -r {} -s 2000-05-01 --inplace -v" \
                .format(AccregexTest.reg_doc_example, AccregexTest.parking_fee_rule_json))
        from accregex import Env
        Env.choose_main(extra_argv)

    def testParkingChargeChanged(self):
        self.instrument_main()

        try:
            from accregex import Account
            from accregex.AccountUtil import gnc_numeric_to_python_Decimal
            session = Account.sessionForFile(AccregexTest.reg_doc_example)
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



