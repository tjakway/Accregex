TEST_DIR=tests/
TEST_CLASSES=TestReadSimpleJSON TestReadAccountData

check:
	cd $(TEST_DIR) && python -munittest $(TEST_CLASSES)
