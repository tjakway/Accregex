TEST_DIR=tests/
TEST_CLASSES=TestReadSimpleJSON TestReadAccountData

.PHONY: .all
.all: check

#delete gnucash lock files
#they can make the tests fail
remove_locks:
	find tests/res/ -name "*.LNK" -o -name "*.LCK" -o -name "*.log" -delete

check: remove_locks
	cd $(TEST_DIR) && python -munittest $(TEST_CLASSES)
