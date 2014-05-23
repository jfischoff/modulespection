#modulespection 
###Template Haskell Module Introspection 

Collect all of the declarations in a module using Template Haskell (via the GHC API). 
 
One can either get all the names, or just the declarations (only type declarations
are supported right now).

Here is a quick example:
```haskell 
import Language.Haskell.TH.Module.Magic (names)

data Test = Test Int
newtype OtherTest = OtherTest Test

someFunction :: String -> String
someFunction = id

-- 'names' is Template Haskell function that will collect all of the 
-- toplevel declaration names of the current file.
names >>= runIO . print >> return []
```

Which will spew the following when compiling:

```
[Test,OtherTest,someFunction]
```

There is also `declarations` which can be used, for example, to make sure that all
types have `ToJSON`/`FromJSON` instances. 

```haskell
import Data.Aeson.TH (deriveJSON, defaultOptions)
import MonadUtils (concatMapM)
import Language.Haskell.TH.Module.Magic (names)

data Test = Test Int
newtype OtherTest = OtherTest Test

concatMapM (deriveJSON defaultOptions) =<< names
```

Which will make JSON instances for `Test`, `OtherTest` and any other types
added to the file.

You can also do the same thing for an existing module.

```haskell
import Data.Aeson.TH (deriveJSON, defaultOptions)
import MonadUtils (concatMapM)
import Language.Haskell.TH.Module.Magic (moduleNames)
import Data.Monoid

concatMapM (deriveJSON defaultOptions) =<< moduleNames "Data.Monoid"
```

Which will build instances for all the types in `Data.Monoid`.
