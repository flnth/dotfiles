c = get_config()

# themes
c.InteractiveShell.highlighting_style = 'legacy'
c.InteractiveShell.colors = 'linux'

# execute lines on startup
c.InteractiveShellApp.exec_lines = [
    'import numpy as np;'
    'import matplotlib.pyplot as plt'
]

# execute scripts on startup
#filename = os.environ.get('PYTHONSTARTUP')
#if filename and os.path.isfile(filename):
#c.TerminalIPythonApp.exec_files = [filename]

# no newlines after input and output
c.TerminalInteractiveShell.separate_in = ''
c.TerminalInteractiveShell.separate_out = ' '
c.TerminalInteractiveShell.separate_out2 = ' '

# vi-mode
c.TerminalInteractiveShell.editing_mode='vi'

# no prompts on closing
c.TerminalInteractiveShell.confirm_exit = False

# Activate greedy completion
#
# This will enable completion on elements of lists, results of function calls,
# etc., but can be unsafe because the code is actually evaluated on TAB.
c.IPCompleter.greedy = False

# Output prompt. '\#' will be transformed to the prompt number
#c.PromptManager.out_template = ''
# Continuation prompt.
# c.PromptManager.in2_template = '... '
# # If True (default), each prompt will be right-aligned with the preceding one.
# c.PromptManager.justify = True
# # Input prompt.  '\#' will be transformed to the prompt number
# c.PromptManager.in_template = '>  '
# c.TerminalInteractiveShell.prompt_in1 = '> '
# c.TerminalInteractiveShell.prompt_out = ''

# autoindent can cause problems on pasting multi-line indented code
c.TerminalInteractiveShell.autoindent = False

# automatically call pdb after every exception
# c.TerminalInteractiveShell.pdb = True

## -----------------------------------------------------------------------------
##      Custom prompt
## -----------------------------------------------------------------------------

from IPython.terminal.prompts import Prompts, Token

class MyPrompt(Prompts):
    def in_prompt_tokens(self, cli=None):
        return [(Token.Prompt, '> ')]

c.TerminalInteractiveShell.prompts_class = MyPrompt


## -----------------------------------------------------------------------------
##       Highlighting
## -----------------------------------------------------------------------------

####### Ancestor
