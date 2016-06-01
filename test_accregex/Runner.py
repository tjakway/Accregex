def get_all_test_suites():
    from unittest import TestSuite
    import TestChangeParking
    import TestReadAccountData
    import TestReadSimpleJSON

    return TestSuite([  TestReadSimpleJSON.get_test_suite(),
                        TestChangeParking.get_test_suite(),
                        TestReadAccountData.get_test_suite()])
