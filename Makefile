TEST_DIR=tests/

check:
	cd $(TEST_DIR) && python -munittest ReadSimpleJSON
