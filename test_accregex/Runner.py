def run_all_test_suites():
    import unittest

    #load tests directly from modules
    import TestReadSimpleJSON
    import TestChangeParking
    import TestReadAccountData
    test_modules = [TestReadSimpleJSON,
                    TestChangeParking,
                    TestReadAccountData]
    test_loader = unittest.defaultTestLoader
    combined_test_suite = unittest.TestSuite(map(test_loader.loadTestsFromModule, test_modules))
    text_test_runner = unittest.TextTestRunner()
    text_test_runner.run(combined_test_suite)
    
