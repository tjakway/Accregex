class SplitTransactionsNotSupportedException(BaseException):
    def __init__(self,*args,**kwargs):
        BaseException.__init__(self,*args,**kwargs)
