module Talos.Strategy.Boltzmann.Polynomial (
    Polynomial(..), bimap, term, constantTerm,
    zero, constant, variable, monomial, (^.),
    assertConstant,
    evalPoly, evalConstant,
    derivatives,
    ) where

import Data.Map (Map)
import Data.MultiSet (MultiSet)

import qualified Data.Map as M
import qualified Data.MultiSet as MS

import Talos.Strategy.Boltzmann.Util

newtype Polynomial coeff var = Polynomial
    { terms :: Map (MultiSet var) coeff }
    -- TODO: should these instances ignore zero coefficients?
    deriving (Eq, Ord, Read, Show)

instance (Eq coeff, Num coeff, PP coeff, PP var) => PP (Polynomial coeff var) where
    ppPrec n (Polynomial ts) = case ppTerms of
        [] -> text "0"
        _ -> (if n > 6 then parens else id) . hcat . punctuate (text " + ") $ ppTerms
        where
        ppTerms = concat
            [ case (coeff == 0, MS.null pows, coeff == 1) of
                (True, _, _) -> []
                (_, True, _) -> [ppPrec 6 coeff]
                (_, False, False) -> [ppPrec 7 coeff <> text "*" <> ppPows 7 pows]
                (_, False, True ) -> [ppPows 6 pows]
            | (pows, coeff) <- M.toAscList ts
            ]

        ppPows prec pows = hcat . punctuate (text "*") $
            [ if pow > 1
                then ppPrec 8 var <> text "^" <> pp pow
                else ppPrec prec var
            | (var, pow) <- MS.toAscOccurList pows
            ]

instance (Num coeff, Ord var) => Num (Polynomial coeff var) where
    fromInteger = constant . fromInteger
    Polynomial p + Polynomial p' = Polynomial (M.unionWith (+) p p')
    Polynomial p * Polynomial p' = Polynomial $ M.fromListWith (+)
        [ (MS.union pows pows', coeff * coeff')
        | (pows , coeff ) <- M.toList p
        , (pows', coeff') <- M.toList p'
        ]
    negate (Polynomial p) = Polynomial (negate <$> p)
    abs = polyUnimplemented "abs"
    signum = polyUnimplemented "signum"

polyUnimplemented :: HasCallStack => String -> a
polyUnimplemented = unimplemented . ("Polynomial::"++)

term :: (Num coeff, Ord var) => Polynomial coeff var -> MultiSet var -> coeff
term p vs = M.findWithDefault 0 vs (terms p)

constantTerm :: (Num coeff, Ord var) => Polynomial coeff var -> coeff
constantTerm p = term p MS.empty

zero :: Polynomial coeff var
zero = Polynomial M.empty

constant :: coeff -> Polynomial coeff var
constant = flip monomial MS.empty

variable :: Num coeff => var -> Polynomial coeff var
variable = monomial 1 . MS.singleton

monomial :: coeff -> MultiSet var -> Polynomial coeff var
monomial c v = Polynomial (M.singleton v c)

(^.) :: (Num coeff, Ord var) => var -> Int -> Polynomial coeff var
x ^. e = monomial 1 (MS.insertMany x e MS.empty)

-- TODO: this could probably be made more efficient
evalPoly :: (Num coeff, Ord var') => (var -> Either var' coeff) -> Polynomial coeff var -> Polynomial coeff var'
evalPoly env p = sum
    [ product $ constant coeff :
        [ case env var of
            Left var' -> var' ^. pow
            Right coeff' -> constant (coeff' ^ pow)
        | (var, pow) <- MS.toOccurList vars
        ]
    | (vars, coeff) <- M.toList (terms p)
    ]

assertConstant :: (Eq coeff, Num coeff, Ord var) => Polynomial coeff var -> Maybe coeff
assertConstant p = case [(vars, coeff) | (vars, coeff) <- M.toList (terms p), coeff /= 0] of
    [] -> Just 0
    [(vars, coeff)] | MS.null vars -> Just coeff
    _ -> Nothing

evalConstant :: (Eq coeff, Num coeff, Ord var) => Map var coeff -> Polynomial coeff var -> Maybe coeff
evalConstant env p = assertConstant $ evalPoly findVar p where
    findVar var = case M.lookup var env of
        Nothing -> Left ()
        Just val -> Right val

derivatives :: (Num coeff, Ord var) => Polynomial coeff var -> Map var (Polynomial coeff var)
derivatives = M.unionsWith (+) . map monoDerivatives . M.toList . terms where
    monoDerivatives (vars, coeff) = M.mapWithKey
        (\var pow -> monomial (fromIntegral pow * coeff) (MS.delete var vars))
        (MS.toMap vars)

bimap :: (Num coeff', Ord var') => (coeff -> coeff') -> (var -> var') -> Polynomial coeff var -> Polynomial coeff' var'
bimap fCoeff fVar (Polynomial ts) = Polynomial $ M.fromListWith (+)
    [ (MS.map fVar vars, fCoeff coeff)
    | (vars, coeff) <- M.toList ts
    ]
