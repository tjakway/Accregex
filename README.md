***WARNING***
Do NOT run this program while GnuCash is open.
From the GnuCash [docs](http://svn.gnucash.org/docs/head/python_bindings_page.html):
"Python-scripts should not be executed while GnuCash runs. GnuCash is designed as a single user application with only one program accessing the data at one time. You can force your access but that may corrupt data. Maybe one day that may change but for the moment there is no active development on that."


Dependencies:
- GnuCash python bindings
- gnucash-env (should be installed with your copy of GnuCash)
- python 2.7

On Ubuntu:
```
sudo apt-get install gnucash python-gnucash python2.7
```

[MIT Licensed](https://opensource.org/licenses/MIT)
