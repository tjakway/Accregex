if __name__ == '__main__':
    import unittest
    from .Runner import get_all_test_suites 
    runner = unittest.TextTestRunner()
    runner.run(get_all_test_suites())
