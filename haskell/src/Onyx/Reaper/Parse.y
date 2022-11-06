{
module Onyx.Reaper.Parse (parseStack) where

import Onyx.Reaper.Scan
import Onyx.Reaper.Base (Element(..))
import qualified Data.Text as T
import Onyx.StackTrace (StackTraceT, fatal, inside)
}

%name parseEither
%tokentype { (AlexPosn, Token) }
%error { parseError }
%monad { Either [(AlexPosn, Token)] }

%token
  nl { (_, Newline) }
  '<' { (_, AngleL) }
  '>' { (_, AngleR) }
  atom { (_, Atom $$) }

%%

File : Newlines Element Newlines { $2 }

Element : '<' atom Atoms Newlines1 Elements '>' { Element $2 $3 (Just $5) }
        | atom Atoms                            { Element $1 $2 Nothing   }

-- 0 or more
Elements : Element Newlines1 Elements { $1 : $3 }
         |                            { []      }

-- 0 or more
Newlines :             { () }
         | nl Newlines { () }

-- 1 or more
Newlines1 : nl Newlines { () }

-- 0 or more
Atoms :            { []      }
      | atom Atoms { $1 : $2 }

{

-- | If instead of this error, "Internal Happy error" is sometimes printed, make
-- sure you are using Happy 1.18.7 or later.
parseError :: e -> Either e a
parseError = Left

showError :: [(AlexPosn, Token)] -> String
showError [] = "Parse error at end of file"
showError ((AlexPn _ ln col, tok) : _) =
  "Parse error at " ++ show ln ++ ":" ++ show col ++ ", token " ++ show tok

fatalError :: (Monad m) => [(AlexPosn, Token)] -> StackTraceT m a
fatalError [] = inside "End of file" $ fatal "Parse error"
fatalError ((AlexPn _ ln col, tok) : _)
  = inside ("Line " ++ show ln ++ ", column " ++ show col)
  $ inside ("Token " ++ show tok) -- TODO this should probably have a length limit
  $ fatal "Parse error"

parse :: [(AlexPosn, Token)] -> Element
parse = either (error . showError) id . parseEither

parseStack :: (Monad m) => [(AlexPosn, Token)] -> StackTraceT m Element
parseStack = either fatalError return . parseEither

}
