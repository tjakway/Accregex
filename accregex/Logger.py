
#this logging class doesn't actually print anything
#it just stores a string (thus no side effects)
class Logger:
    _log_string = ""
    
    def write(self, write_str):
        self._log_string += str(write_str)
        self._log_string += '\n'

    def get_log(self):
        return _log_string
