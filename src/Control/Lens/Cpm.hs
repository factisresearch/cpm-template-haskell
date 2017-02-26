module Control.Lens.Cpm
    ( makeLenses
    )
where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Control.Lens hiding (lensRules, makeLenses)
import qualified Control.Lens as L

----------------------------------------
-- STDLIB
----------------------------------------
import Language.Haskell.TH


makeLenses ::  Name -> Q [Dec]
makeLenses = makeLensesWith lensRules

lensRules :: LensRules
lensRules = L.lensRules & lensField .~ (\_tyName _ s -> [TopName (mkName (nameBase s ++ "L"))])
