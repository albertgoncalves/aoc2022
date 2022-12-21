import Data.Char (isDigit, isLower)
import Data.Map ((!))
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch,
    munch1,
    readP_to_S,
    satisfy,
    string,
    (<++),
  )
import Text.Printf (printf)

data Expr
  = ExprInt Int
  | ExprOp Expr Char Expr
  | ExprVar String

instance Show Expr where
  show (ExprInt n) = show n
  show (ExprOp l c r) = printf "(%s %c %s)" (show l) c (show r)
  show (ExprVar s) = s

int :: ReadP Int
int = (read .) . (:) <$> (satisfy isDigit <++ char '-') <*> munch isDigit

ident :: ReadP String
ident = munch1 isLower

var :: ReadP Expr
var = ExprVar <$> ident

space :: ReadP Char
space = char ' '

expr :: ReadP Expr
expr =
  (ExprInt <$> int)
    <++ ( ExprOp
            <$> (var <* space)
            <*> (satisfy (`elem` "+-*/") <* space)
            <*> var
        )

pair :: ReadP (String, Expr)
pair = (,) <$> (ident <* string ": ") <*> expr

op :: Char -> Int -> Int -> Int
op '+' = (+)
op '-' = (-)
op '*' = (*)
op '/' = div
op _ = undefined

part1 :: M.Map String Expr -> String
part1 = show . eval "root"
  where
    eval :: String -> M.Map String Expr -> Int
    eval x m =
      case m ! x of
        ExprInt n -> n
        ExprOp (ExprVar l) c (ExprVar r) -> op c (eval l m) (eval r m)
        _ -> undefined

revOp :: Char -> Int -> Int -> Int
revOp '+' = (-)
revOp '-' = (+)
revOp '*' = div
revOp '/' = (*)
revOp _ = undefined

solve :: Int -> Expr -> Expr
solve n (ExprOp (ExprInt l) '-' r) = solve (l - n) r
solve n (ExprOp (ExprInt l) '/' r) = solve (l `div` n) r
solve n (ExprOp (ExprInt l) c r) = solve (revOp c n l) r
solve n (ExprOp l c (ExprInt r)) = solve (revOp c n r) l
solve n (ExprVar "humn") = ExprInt n
solve _ _ = undefined

part2 :: M.Map String Expr -> String
part2 =
  show
    . eval "root"
    . M.insert "humn" (ExprVar "humn")
    . M.adjust f "root"
  where
    f (ExprOp l _ r) = ExprOp l '=' r
    f _ = undefined

    eval :: String -> M.Map String Expr -> Expr
    eval x m =
      case M.lookup x m of
        Nothing -> ExprVar x
        Just (ExprOp (ExprVar l) c (ExprVar r)) ->
          case (eval l m, c, eval r m) of
            (e, '=', ExprInt n) -> solve n e
            (_, '=', _) -> undefined
            (ExprInt a, _, ExprInt b) -> ExprInt $ op c a b
            (a, _, b) -> ExprOp a c b
        Just e -> e

main :: IO ()
main =
  interact $
    unlines
      . zipWith ($) [part1, part2]
      . repeat
      . M.fromList
      . fst
      . head
      . readP_to_S (many1 (pair <* char '\n') <* eof)
