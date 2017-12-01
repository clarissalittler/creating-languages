import Control.Monad.State
import Data.Char (isSpace)
import Text.Parsec


type Name = String

type Env a = [(Name,a)]

type Id = Int

data InterpState = IS {heap :: (Id -> Val),
		       heapNumber :: Id,
		       envChain :: [Env Id]}

initState = IS emptyHeap 0 []

emptyHeap = \_ -> error "undefined memory location"

emptyEnv = [[]]

type Interp = StateT InterpState IO

data Val = VNum Int | VFun [Name] Exp [Env Id]

data Exp = DeclVar Name Exp
         | SetVar Name Exp
         | UseVar Name
         | If Exp Exp Exp
         | Begin [Exp]
         | While Exp Exp
         | Function [Name] Exp
         | Funcall Exp Exp
         | Op BinOp Exp Exp
         | Print Exp
	 | ELit Int

data BinOp = Add | Sub | Lt | Eq

liftBinop :: (Int -> Int -> Int) -> Val -> Val -> Val
liftBinop op (VNum i) (VNum j) = VNum $ op i j
liftBinop op _ _ = error "can't apply numeric operations to functions"

lessThan :: Int -> Int -> Int
lessThan i j | i < j = 1
             | otherwise = 0

equal :: Int -> Int -> Int
equal i j | i == j = 1
          | otherwise = 0

evalBinop :: BinOp -> Val -> Val -> Val
evalBinop Add = liftBinop (+)
evalBinop Sub = liftBinop (-)
evalBinop Lt = liftBinop lessThan
evalBinop Eq = liftBinop equal

update :: a -> b -> (a -> b) -> a -> b
update a b f x = if x == a
                 then b
                 else f x

lookupVar :: Name -> [Env Id] -> Maybe Id
lookupVar n [] = Nothing
lookupVar n (e:es) = case lookup n e of
		       Just i -> Just i
		       Nothing -> lookupVar n es

makeVar :: Name -> Val -> Interp Int
makeVar n v = do
  IS h hn ec <- get
  let h' = update hn v h
      ec' = ((n,hn):(head ec)):(tail ec)
  put (IS h' (hn + 1) ec')
  return hn

updateVar :: Name -> Val -> Interp ()
updateVar n v = do
  IS h hn ec <- get
  case lookupVar n ec of
    Nothing -> error "Variable not in scope"
    Just heapNum -> put (IS (update heapNum v h) hn ec) 

useVar :: Name -> Interp Val
useVar n = do
  h <- gets heap
  ec <- gets envChain
  case lookupVar n ec of
    Nothing -> error "Not a declared variable"
    Just i -> return $ h i

newScope :: Interp ()
newScope = do
  IS h hn ec <- get
  put $ IS h hn ([] : ec)

popScope :: Interp ()
popScope = do
  IS h hn ec <- get
  put $ IS h hn (tail ec)

tempScope :: Interp a -> Interp a
tempScope m = do
  IS h hn ec <- get
  m
  IS h' hn' _ <- get
  put h' hn' ec

eval :: Exp -> Interp Val
eval (DeclVar n e) = do
  v <- eval e
  makeVar n v
eval (SetVar n e) = do
  v <- eval e
  updateVar n v
  return v
eval (UseVar n) = useVar n
eval (If cond e1 e2) = do
  vc <- eval cond
  case vc of
    VNum i -> if i > 0
                then eval e1
                else eval e2
    _ -> error "Tried to use a function as a number"
eval (Begin es) = do
  vs <- mapM eval es
  return $ last vs
eval (While c e) = do
  vc <- eval c
  case vc of
    VNum i -> if i > 0
                then do
                   eval e
                   eval (While c e)
                else return $ VNum 0
    _ -> error "Can't use a function as a number"
eval (Function ns e) = do
  ec <- gets envChain
  return $ VFun ns e ec
eval (Funcall e es) = do
  -- we're doing something inefficient here by putting our called variables on the heap
  -- it's not t e c h n i c a l l y wrong but in future interpreters we'll get into why this should be improved and how it doesn't reflect the actual way compiled code behaves
  vs <- mapM eval es
  f <- eval e
  case f of
    VFun ns body env -> tempScope $ do
      IS h hn _ <- get
      put (IS h hn env)
      newScope
      mapM (uncurry makeVar) (zip ns vs)
      eval body
    _ -> error "attempted to call a non function"
eval (Op b e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ evalBinop b v1 v2
eval (Print e) = do
       v <- eval e
       lift $ print v

expParser :: Parser Exp
expParser = eatSpaces $ declVarP
                        <|> setVarP
                        <|> ifP
                        <|> beginP
                        <|> whileP
                        <|> funcP
                        <|> funcallP
                        <|> opP
                        <|> printP
                        <|> litP
                        <|> useVarP

eatSpaces :: Parser a -> Parser a
eatSpaces p = do
  spaces
  x <- p
  spaces
  return x

char' = eatSpaces . char
string' = eatSpaces . string

parens :: Parser a -> Parser a
parens p = do
  char' '('
  x <- p
  char' ')' 
  return x
  
ifP :: Parser Exp
ifP = parens $
  string' "if"
  ec <- expParser
  et <- expParser
  ef <- expParser
  return $ If ec et ef

litP :: Parser Exp
litP = do
  s <- many1 digit
  return $ ELit (read s)

printP :: Parser Exp
printP = parens $ do
  string' "print" 
  e <- expParser
  return $ Print e

beginP :: Parser Exp
beginP = parens $ do
  string' "begin"
  es <- many1 expParser
  return $ Begin es
  
whileP :: Parser Exp
whileP = parens $ do
  string' "while"
  c <- expParser
  b <- expParser
  return $ While c b

opAux n c = do
  string n
  return c

plusP = opAux "+" Add

minP = opAux "-" Sub

ltP = opAux "<" Lt

eqP = opAux "==" Eq

binopP :: Parser BinOp
binopP = plusP <|> minP <|> ltP <|> eqP

nameP :: Parser Name
nameP = eatSpaces $ many1 letter

opP :: Parser Exp
opP = parens $ do
  op <- binopP
  e1 <- expParser
  e2 <- expParser
  return $ Op op e1 e2

funcP :: Parser Exp
funcP = parens $ do
  string' "function"
  ns <- parens $ many1 nameP
  b <- expParser
  return $ Function ns b

funcallP :: Parser Exp
funcallP = parens $ do

declVarP :: Parser Exp
declVarP = parens $ do
  string' "var"
  n <- nameP
  e <- expParser
  return $ DeclVar n e

setVarP :: Parser Exp
setVarP = parens $ do
  string' "set"
  n <- nameP
  e <- expParser
  return $ SetVar n e

useVarP :: Parser Exp
useVarP = liftM UseVar nameP 

{-

-- Now we come to our parser for building our AST

replace :: Eq a => a -> [a] -> [a] -> [a]
replace x rplc [] = []
replace x rplc (a:as) | x == a = rplc ++ replace x rplc as
                      | otherwise = replace x rplc as 

tokenize :: String -> [String]
tokenize = words . replace '(' "( " . replace ')' " )"

-- this function should have a lot more data validation than it actually does
-- you can consider that an "exercise left to the reader" or simply wait until I've added it!!

data Forest a = Leaf a | Branch [Forest a]
  deriving (Eq, Show)

parsing :: [String] -> Exp
parsing [] = error "this shouldn't happen"
parsing tokens = if (head tokens) == "("
                    then let subExps = takeWhile (/= ")") (tail tokens)
                         in  

sExpToExp' :: String -> [String] -> Exp
sExpToExp' "begin" sexps = Begin (map sExpToExp sexps)
sExpToExp' "while" sexps = While (sExpToExp $ head sexps)
sExpToExp' "if" sexps = If (sExpToExp $ sexps !! 0) (sExpToExp $ sexps !! 1) (sExpToExp $ sexps !! 2)
sExpToExp' "print" sexps = Print (sExpToExp $ head sexps)
sExpToExp' "set" sexps = SetVar (sexps !! 0) (sExpToExp $ sexps !! 1)
sExpToExp' "function" sexps = Function (head sexps) (sExpToExp $ sexps !! 1)
sExpToExp' "var" sexps = DeclVar (sexps !! 0) (sexps !! 1)
sExpToExp' firstElem sexps = if firstElem `elem` ["==","+","-","<"]
                               then Op firstElem (sexps !! 0) (sexps !! 1)
                               else -- if it's not an operation, then it has to be a variable or a literal
                                 if all isLetter
                                   then UseVar firstElem
                                   else ELit (read $ firstElem)

sExpToExp :: [String] -> Exp
sExpToExp [] = error "that shouldn't happen"
sExpToExp ss = sExpToExp' (head ss) (tail ss)

fromTokens :: [String] -> [Forest String]
fromTokens [] = error "that's not right"
fromTokens tokens = if (head tokens) == "("
                       then let sExps = fromTokens $ takeWhile (/= ")") (tail tokens)
                                rest  = fromTokens $ tail $ dropWhile (/= ")") tokens)
                            in
                       else [Leaf $ head $ tokens] 

-}
