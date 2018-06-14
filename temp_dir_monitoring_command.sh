#!/usr/bin/env bash

echo "# of temporary dirs used: " $(ls /tmp/ | grep accregex | wc -l) ; echo ; df -h | grep -P "(tmpfs|Filesystem)" | awk '{ print $1, $5}'
