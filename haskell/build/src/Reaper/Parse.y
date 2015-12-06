{
module Reaper.Parse (parse) where

import qualified Reaper.Scan as L
import Reaper.Base (Element(..))
}

%name parse
%tokentype { (L.AlexPosn, L.Token) }
%error { parseError }

%token
  nl { (_, L.Newline) }
  '<' { (_, L.AngleL) }
  '>' { (_, L.AngleR) }
  atom { (_, L.Atom $$) }

%%

File : Element Newlines { $1 }

Element : '<' atom Atoms Newlines Elements '>' { Element $2 $3 (Just $5) }
        | atom Atoms                           { Element $1 $2 Nothing   }

-- 0 or more
Elements : Element Newlines Elements { $1 : $3 }
         |                           { []      }

-- 1 or more
Newlines : nl Newlines { () }
         | nl          { () }

-- 0 or more
Atoms :            { []      }
      | atom Atoms { $1 : $2 }

{

parseError :: [(L.AlexPosn, L.Token)] -> a
parseError [] = error "Parse error at EOF"
parseError ((L.AlexPn _ ln col, tok) : _) = error $
  "Parse error at " ++ show ln ++ ":" ++ show col ++ ", token " ++ show tok

}
