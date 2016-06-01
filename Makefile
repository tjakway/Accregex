#get the path to the makefile (does not depend on realpath)
#see http://stackoverflow.com/questions/322936/common-gnu-makefile-directory-path
#specifically http://stackoverflow.com/questions/322936/common-gnu-makefile-directory-path#comment11704496_324782
#(the comment by @Xavier Holt (http://stackoverflow.com/users/589985/xavier-holt)
TOP := $(dir $(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST)))
TEST_PACKAGE=test_accregex
TEST_DIR=$(TOP)/$(TEST_PACKAGE)
RES_DIR=$(TEST_DIR)/res

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
	cd $(TOP) && python -m $(TEST_PACKAGE)

.PHONY: clean
clean: remove_locks rm_numbered_gnucash_files rm_gnucash_backups
	find . -name "*.pyc" -type f -delete
