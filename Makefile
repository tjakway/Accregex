TEST_DIR=accregex_tests/
RES_DIR=$(TEST_DIR)/res
TEST_CLASSES=TestReadSimpleJSON TestReadAccountData TestChangeParking

.PHONY: .all
all: check

.PHONY: rm_gnucash_backups
rm_gnucash_backups:
	find $(RES_DIR) -name "*.gnucash.bak" -delete

.PHONY: rm_numbered_gnucash_files
rm_numbered_gnucash_files:
	find $(RES_DIR) -regextype sed -regex ".*\.gnucash\.[0-9]\+\.gnucash" -delete

#delete gnucash lock files
#they can make the tests fail
.PHONY: remove_locks
remove_locks: 
	find $(RES_DIR) -regex ".*\.LNK\|.*\.LCK\|.*\.log" -delete
	
.PHONY: check
check: remove_locks
	cd $(TEST_DIR) && python -munittest $(TEST_CLASSES)

.PHONY: clean
clean: remove_locks rm_numbered_gnucash_files rm_gnucash_backups
	find . -name "*.pyc" -type f -delete
