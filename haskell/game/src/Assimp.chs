module Assimp where

import Foreign
import Foreign.C

#include "assimp/cimport.h"
#include "assimp/scene.h"
#include "assimp/postprocess.h"

{#pointer *aiScene as AiScene newtype #}

peekMaybeString :: CString -> IO (Maybe String)
peekMaybeString str = if str == nullPtr
  then return Nothing
  else Just <$> peekCString str

enumOr :: (Enum a, Integral i) => [a] -> i
enumOr xs = fromIntegral $ foldr (.|.) 0 $ map fromEnum xs

{#fun aiImportFile
  { withCString* `FilePath'
  , enumOr `[AiPostProcessSteps]'
  } -> `AiScene'
#}

{#fun aiReleaseImport
  { `AiScene'
  } -> `()'
#}

{#fun aiGetErrorString
  {} -> `Maybe String' peekMaybeString*
#}

{#enum aiPostProcessSteps as AiPostProcessSteps {upcaseFirstLetter}
  deriving (Eq, Show)
#}
