#This file is a stripped down intermediate state between Norvig's lispy implementation
#and a full AST based approach
#here we're showing how to modify the lispy interpreter to be the very small language
#PangolinV1
#You can also look at the Pangolin1AST.py file for an example of how to implement Pangolinv1 with an actual
#abstract syntax tree like we discussed in our articles. The AST is definitely bigger and more complicated  of code, but it will also be easier to analyze and play with once we start dealing with types!

# for now, though, if you want to try changing this language you easily can!
# you can add function to the standard environment by changing the standard_env function
# you can also add syntax by changing the eval function
# Things to try:
# + adding a for-loop
# + adding elseif clauses
# + adding a distinct bool type that isn't interchangeable with integers
import operator as op

Symbol = str
List = list
Number = int

def tokenize(chars):
    return chars.replace('(', ' ( ').replace(')', ' ) ').split()

def parse(program):
    return read_from_tokens(tokenize(program))

def read_from_tokens(tokens):
    if len(tokens) == 0:
        raise SyntaxError('unexpected end of file')
    token = tokens.pop(0)
    if '(' == token:
        L = []
        while tokens[0] != ')':
            L.append(read_from_tokens(tokens))
        tokens.pop(0)
        return L
    elif ')' == token:
        raise SyntaxError('unexpected )')
    else:
        return atom(token)

def atom(token):
    try: return int(token)
    except ValueError:
        return Symbol(token)


class Env(dict):
    "An environment: a dict of {'var':val} pairs, with an outer Env."
    def __init__(self, parms=(), args=(), outer=None):
        self.update(zip(parms, args))
        self.outer = outer
    def find(self, var):
        "Find the innermost Env where var appears."
        return self if (var in self) else self.outer.find(var)

def standard_env():
    env = Env()
    env.update({'+':op.add, '-':op.sub,
        '<':op.lt, '==':op.eq, 
        'begin':   lambda *x: x[-1],
    })
    return env

class Procedure(object):
    "A user-defined Scheme procedure."
    def __init__(self, parms, body, env):
        self.parms, self.body, self.env = parms, body, env
    def __call__(self, *args): 
        return eval(self.body, Env(self.parms, args, self.env))

global_env = standard_env()

def eval(x, env=global_env):
    if isinstance(x, Symbol):
        return env.find(x)[x]
    elif not isinstance(x, List):
        return x
    elif x[0] == 'if':
        (_, test, conseq, alt) = x
        exp = (conseq if eval(test, env) else alt)
        return eval(exp, env)
    elif x[0] == 'var':
        (_, var, exp) = x
        env[var] = eval(exp, env)
    elif x[0] == 'set':
        (_, var, exp) = x
        env.find(var)[var] = eval(exp, env)
    elif x[0] == 'function':
        (_, parms, body) = x
        return Procedure(parms, body, env)
    elif x[0] == 'while':
        (_,cond,body) = x
        while eval(cond,env):
            eval(body,env)
    else:
        proc = eval(x[0], env)
        args = [eval(arg, env) for arg in x[1:]]
        return proc(*args)

def repl(prompt='pangolin1> '):
    "A prompt-read-eval-print loop."
    while True:
        val = eval(parse(input(prompt)))
        if val is not None: 
            print(schemestr(val))

def schemestr(exp):
    "Convert a Python object back into a Scheme-readable string."
    if isinstance(exp, List):
        return '(' + ' '.join(map(schemestr, exp)) + ')' 
    else:
        return str(exp)
