#the following code is taken from account_analysis.py of the python-gnucash Ubuntu repository.
#original license text follows.

# account_analysis.py -- Output all the credits and debits on an account
#
# Copyright (C) 2009, 2010 ParIT Worker Co-operative <transparency@parit.ca>
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, contact:
# Free Software Foundation           Voice:  +1-617-542-5942
# 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
# Boston, MA  02110-1301,  USA       gnu@gnu.org
#
# @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>


from decimal import Decimal
from math import log10
from gnucash import Session, GncNumeric, Split

def gnc_numeric_to_python_Decimal(numeric):
    negative = numeric.negative_p()
    if negative:
        sign = 1
    else:
        sign = 0
    copy = GncNumeric(numeric.num(), numeric.denom())
    result = copy.to_decimal(None)
    if not result:
        raise Exception("gnc numeric value %s can't be converted to decimal" %
                        copy.to_string() )
    digit_tuple = tuple( int(char)
                         for char in str(copy.num())
                         if char != '-' )
    denominator = copy.denom()
    exponent = int(log10(denominator))
    assert( (10 ** exponent) == denominator )
    return Decimal( (sign, digit_tuple, -exponent) )


