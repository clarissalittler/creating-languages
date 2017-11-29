# we're structuring this code a little differently than in Norvig's lispy example because we're building actual abstract syntax trees. This involves more code initially than Norvig's code but that's because we're going to need ASTs later on as we build the language and add features, syntax, and type checking in various forms.

# Once we have our abstract syntax tree, we evaluate our program with the eval method that each class implements

#we're using a modified version of the parsing code from Lispy that instead builds an actual syntax tree rather than leaving it as lists-of-lists

#Tokenization is still done by splitting the long string into a list of tokens.
#The easy way to do that is by taking all the parens and making sure spaces are inserted. This means that
#something like (+ 1 1) is going to become the list of tokens [(, +, 1, 1, )]
#which is going to get turned into the list of lists [[+, 1, 1]]
#that in turns need to get turned into BuiltIn("+",Lit(1),Lit(1))
def tokenize(chars):
    return chars.replace('(', ' ( ').replace(')', ' ) ').split()

def parse(program):
    return sExpToAST(read_from_tokens(tokenize(program)))

operatorTokens = ["+","-","<","=="]

#sExpToAST is the new parser for our interpreter.
#this is where the bulk of our new work is done
#we have a class corresponding to each piece of syntax

def sExpToAST(listOfTokens):
    if type(listOfTokens) is list:
        firstElem = listOfTokens.pop(0)
        #Built-in binary operators. All our operations require exactly two arguments in pangolinv1
        if firstElem in operatorTokens:
            if length(listOfTokens) > 3:
                raise SyntaxError("too many arguments to built in function")
            left = sExpToAST(listOfTokens[1])
            right = sExpToAST(listOfTokens[2])
            return BuiltIn(firstElem,left,right)
        #The begin operation that's used to sequence expressions of code.
        elif firstElem == "begin":
            return Begin([sExpToAST(s) for s in listOfTokens])
        #While loop
        elif firstElem == "while":
            cond = sExpToAKST(listOfTokens[1])
            return While(cond,sExpToAST(listOfTokens[2]))
        #If expression
        elif firstElem == "if":
            cond = sExpToAST(listOfTokens[1])
            tBranch = sExpToAST(listOfTokens[2])
            fBranch = sExpToAST(listOfTokens[3])
            return If(cond,tBranch,fBranch)
        #Print expression
        elif firstElem == "print":
            return Print(sExpToAST(listOfTokens[1]))
        #Setting variables
        elif firstElem == "set":
            return SetVar(listOfTokens[1],sExpToAST(listOfTokens[2]))
        #Creating functions
        elif firstElem == "function":
            args = listOfTokens[1]
            body = listOfTokens[2]
            if type(args) is not list:
                raise SyntaxError("Need to provide a list of arguments to function")
            else:
                return FunctionExpr(args,sExpToAST(body))
        #Creating variables
        elif firstElem == "var":
            return DeclVar(listOfTokens[1],sExpToAST(listofTokens[2]))
        #Function calls
        else:
            funExpr = sExpToAST(listOfTokens.pop(0))
            return FunctionExpr(funExpr,[sExpToAST(s) for s in listOfTokens])
    else:
        #here this means that the sExp wasn't actually a list so we need to handle turning it into either a Lit or a Var
        try: return Lit(int(listOfTokens))
        except ValueError:
            #the only other possibility is that it was a variable being used
                return UseVar(listOfTokens)

def read_from_tokens(tokens):
    if len(tokens) == 0:
        raise SyntaxError('beep boop')
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
        #so I decided to take out the atomization of tokens into variables or literals from this part and fold it
        #into the sExpToAST function instead 
        return token

def atom(token):
    try: return int(token)
    except ValueError:
        return str(token)


#borrowing Env definition from Norvig's lispy, in part because it will make it easier to compare code
class Env(dict):
    "An environment: a dict of {'var':val} pairs, with an outer Env."
    def __init__(self, parms=(), args=(), outer=None):
        self.update(zip(parms, args))
        self.outer = outer
    def find(self, var):
        "Find the innermost Env where var appears."
        return self if (var in self) else self.outer.find(var)

class Begin:
    def __init__(self,exprs):
        self.exprs = exprs
    def eval(self,env):
        for e in self.exprs:
            result = e.eval(env)
        return result

class While:
    def __init__(self,cond,code):
        self.cond = cond
        self.code = code
    def eval(self,env):
        while self.cond.eval(env):
            self.code.eval(env)
        return True

class SetVar:
    def __init__(self,name,exp):
        self.name = name
        self.exp = exp
    def eval(self,env):
        v = self.exp.eval(env)
        env.update(self.name,v)

class DeclVar:
    def __init__(self,name,exp):
        self.name = name
        self.exp = exp
    def eval(self,env):
        env[self.name] = self.exp.eval(env) 

class UseVar:
    def __init__(self,name):
        self.name = name
    def eval(self,env):
        env.find(self.name)[self.name]

class BuiltIn:
    def __init__(self,symb,left,right):
        self.symb = symb
        self.left = left
        self.right = right
    def eval(self,env):
        lval = self.left.eval(env)
        rval = self.right.eval(env)
        if type(lval) == Lit && type(rval) == Lit:
            if self.symb == "+":
                return lval.eval(env) + rval.eval(env)
            elif self.symb == "-":
                return lval.eval(env) - rval.eval(env)
            elif self.symb == "<":
                return int(lval < rval) # one convenience Python gives us is that it too uses integers as booleans
            elif self.symb == "==":
                return int(lval == rval)
        else:
            raise Exception("Can't use a function as a number") 

class Print:
    def __init__(self,exp):
        self.exp = exp
    def eval(self,env):
        v = self.exp.eval(env)
        if type(v) == int:
            print(v)
        else:
            print("Function")
        return 1

# this class is being used to store the actual value of the function not the parsing
class FunctionValue:
    def __init__(self,argNames,body,env):
        self.argNames = argNames
        self.body = body
        self.env = env

class FunctionExpr:
    def __init__(self,argNames,body):
        self.argNames = argNames
        self.body = body
    def eval(self,env):
        return FunctionValue(self.argNames,self.body,env)

class FunctionCall:
    def __init__(self,fun,args):
        self.funExpr = fun
        self.args = args
    def eval(self,env):
        bod = self.funExpr.body
        newEnv = Env(self.funExpr.argNames,self.args,env)
        bod.eval(newEnv)
    
class Lit:
    def __init__(self,n):
        self.val = n
    def eval(self,env):
        return self.val

class If:
    def __init__(self,cond,tbranch,fbranch):
        self.cond = cond
        self.tbranch = tbranch
        self.fbranch = fbranch
    def eval(self,env):
        cval = self.cond.eval(env)
        if type(cval) == int:
            if cval >= 1:
                return self.tbranch.eval(env)
            elif:
                return self.fbranch.eval(env)
        else:
            raise Exception("Can't use a function as a number")


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
