{-# LANGUAGE TemplateHaskell #-}
module Data.SafeCopy.Cpm
    (
      SafeCopy'(..)
    , deriveSafeCopy
    , deriveSafeCopyCtx
    , deriveSafeCopyCustomCtx
    , deriveSafeCopyNoCtx
    , oldNewtypeDerivingVersion
    )
where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.SafeCopy hiding (deriveSafeCopy)
import qualified Data.SafeCopy as SC
import Data.Serialize

----------------------------------------
-- STDLIB
----------------------------------------
import qualified Language.Haskell.TH as TH

-- | A variant of the 'SafeCopy' class for types of kind @* -> *@.
class SafeCopy' k where
    safePut' :: SafeCopy a => k a -> Put
    safeGet' :: SafeCopy a => Get (k a)

deriveSafeCopy ::
    Version a -> TH.Name -> TH.Name -> (TH.Q [TH.Dec])
deriveSafeCopy version kindName tyName =
    patchedDeriveSafeCopy version kindName tyName

-- | Derives a SafeCopy instance with additional constraints.
--
-- Example:
--
-- `deriveSafeCopyCtx 1 'base ''Foo (\[k] -> [TH.appT (TH.conT ''Ord) k])`
--
-- derives a SafeCopy instance for `Foo :: * -> *` with an additional
-- `Ord` constraint for `Foo`s type parameter.
deriveSafeCopyCtx :: Version a
                  -> TH.Name
                  -> TH.Name
                  -> ([TH.TypeQ] -> [TH.PredQ])
                  -> (TH.Q [TH.Dec])
deriveSafeCopyCtx version kindName tyName extraPreds =
    deriveSafeCopyCustomCtx version kindName tyName mkCtx
    where
      mkCtx args oldCtx =
          oldCtx ++ extraPreds args

deriveSafeCopyNoCtx ::
       Version a
    -> TH.Name
    -> TH.Name
    -> (TH.Q [TH.Dec])
deriveSafeCopyNoCtx version kindName tyName =
    deriveSafeCopyCustomCtx version kindName tyName (\_ _ -> [])

deriveSafeCopyCustomCtx ::
       Version a
    -> TH.Name
    -> TH.Name
    -> ([TH.TypeQ] -> [TH.PredQ] -> [TH.PredQ])
    -> (TH.Q [TH.Dec])
deriveSafeCopyCustomCtx version kindName tyName extraPreds =
    do decs <- patchedDeriveSafeCopy version kindName tyName
       case decs of
         (TH.InstanceD overlap ctx ty body : _) ->
             do let args = reverse (collectArgs ty)
                newCtx <- sequence (extraPreds (map return args) (map return ctx))
                -- _ <- fail ("args: " ++ show args ++", ty: " ++ show ty)
                return [TH.InstanceD overlap newCtx ty body]
         _ ->
             error $
                 "Unexpected declarations returned by deriveSafeCopy: " ++ show (TH.ppr decs)
    where
      collectArgs :: TH.Type -> [TH.Type]
      collectArgs ty =
          let loop ty =
                  case ty of
                    (TH.AppT l r) ->
                        case l of
                          TH.AppT _ _ -> r : loop l
                          _ -> [r]
                    _ -> []
          in case ty of
               TH.AppT _ r -> loop r
               _ -> []

-- | Derive an instance of `SafeCopy`.
--
-- This is a patched version, which also works for type variables of kind * -> *.
patchedDeriveSafeCopy :: Version a -> TH.Name -> TH.Name -> TH.Q [TH.Dec]
patchedDeriveSafeCopy version kindName tyName =
    do decs <- SC.deriveSafeCopy version kindName tyName
       case decs of
         [TH.InstanceD overlap _ ty body] ->
             do info <- TH.reify tyName
                (context, tyvars) <-
                    case info of
                      TH.TyConI (TH.DataD context _name tyvars _kind _cons _derivs) ->
                          return (context, tyvars)
                      TH.TyConI (TH.NewtypeD context _name tyvars _kind _con _derivs) ->
                          return (context, tyvars)
                      _ -> fail $ "Can't derive SafeCopy instance for: " ++ show (tyName, info)
                return . (:[]) $
                    TH.InstanceD
                    overlap
                    ([safeCopyConstraint var | var <- tyvars] ++ context)
                    ty
                    body
         _ ->
             error $
                 "Unexpected declarations returned by deriveSafeCopy: " ++ show (TH.ppr decs)
   where
     tyVarName :: TH.TyVarBndr -> TH.Name
     tyVarName (TH.PlainTV n) = n
     tyVarName (TH.KindedTV n _) = n

     toSafeCopyClass :: TH.TyVarBndr -> TH.Name
     toSafeCopyClass var =
         case var of
           (TH.PlainTV _) -> ''SafeCopy
           (TH.KindedTV _ (TH.AppT (TH.AppT TH.ArrowT TH.StarT) TH.StarT)) -> ''SafeCopy'
           (TH.KindedTV _ _) -> ''SafeCopy

     safeCopyConstraint :: TH.TyVarBndr -> TH.Type
     safeCopyConstraint var =
         let sfClass = toSafeCopyClass var
         in TH.AppT (TH.ConT sfClass) (TH.VarT $ tyVarName var)

{-

Prior to GHC 7.8, you could use the generalized-newtype-deriving feature
to derive SafeCopy instances.

In GHC 7.8., they fixed a soundness hole in the generalized-newype-deriving
feature (see https://downloads.haskell.org/~ghc/7.8.2/docs/html/users_guide/roles.html).
With this fix, deriving SafeCopy instances for newtypes no longer works.

For example, the following code works with GHC 7.6, but with version 7.8 you get a type error.

newtype Hash = Hash { unHash :: BS.ByteString } deriving (Eq, Ord, Typeable, Generic, SafeCopy)

The type error is:

    Could not coerce from 'Kind BSC.ByteString' to 'Kind Hash'
      because the first type argument of 'Kind' has role Nominal,
      but the arguments 'BSC.ByteString' and 'Hash' differ
      arising from the coercion of the method 'kind' from type
                   'Kind BSC.ByteString' to type 'Kind Hash'

The problem is the method

  kind :: Kind a

from the SafeCopy class. The error message says, that "Kind Hash" and "Kind BS.ByteString"
are different types.

As a solution to this problem, we use TemplateHaskell to derive SafeCopy instances for
the relevant newtypes and add a migrations from the type wrapped in the newtyp to the newtype.
It is important that we pick a version for the newtype that is not used by the wrapped
type. For this, we use the function oldNewtypeDerivingVersion below.

-}
oldNewtypeDerivingVersion :: Version a -> Version a
oldNewtypeDerivingVersion version =
    version + 10000
