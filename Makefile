TEST_DIR=tests/
RES_DIR=$(TEST_DIR)/res
TEST_CLASSES=TestReadSimpleJSON TestReadAccountData

.PHONY: .all
all: check

#delete gnucash lock files
#they can make the tests fail
remove_locks:
	find $(RES_DIR) -regex ".*\.LNK\|.*\.LCK\|.*\.log" -delete
	

check: remove_locks
	cd $(TEST_DIR) && python -munittest $(TEST_CLASSES)
