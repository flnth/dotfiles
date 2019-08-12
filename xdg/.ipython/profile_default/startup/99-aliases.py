import sys
import subprocess
import os

from IPython.core import magic_arguments
from IPython.core.magic import line_magic, cell_magic, line_cell_magic, Magics, magics_class

def z_fun(arg=""):
    path_ = os.popen("fasd -de 'printf %s' " + arg).read().strip()
    if(path_):
        os.chdir(path_)

@magics_class
class ZMagics(Magics):
    @line_magic
    def z(self, line):
        z_fun(line)
    def exit(self, line):
        print("my exit called with " + line)

ip = get_ipython()
ip.register_magics(ZMagics)
