module Talos.Strategy.Boltzmann.Newton where

import Data.Map (Map)
import Data.Maybe
import Numeric.LinearAlgebra ((><))

import qualified Data.Map as M
import qualified Numeric.LinearAlgebra as LA

import Talos.Strategy.Boltzmann.Polynomial (Polynomial(..))
import qualified Talos.Strategy.Boltzmann.Polynomial as P

data NewtonEnvironment var = NewtonEnvironment
    { objectives :: [Polynomial Double var]
    , derivss :: [Map var (Polynomial Double var)] -- invariant: same length as previous
    , dimensionality :: Int -- invariant: is the length of the previous
    } deriving (Eq, Ord, Read, Show)

createEnvironment :: Ord var => [Polynomial Double var] -> NewtonEnvironment var
createEnvironment ps = NewtonEnvironment
    { objectives = ps
    , derivss = map P.derivatives ps
    , dimensionality = length ps
    }

zeroStep :: Ord var =>
    NewtonEnvironment var ->
    Map var Double ->
    Map var Double
zeroStep ne estimate = M.fromList $ zipWith
    (\(var, val) [d] -> (var, val-d))
    (M.toAscList estimate)
    (LA.toLists delta)
    where
    delta = LA.linearSolveSVD jacobian err
    err = (dimensionality ne >< 1)
        [ fromJust (P.evalConstant estimate obj)
        | obj <- objectives ne
        ]
    jacobian = (dimensionality ne >< M.size estimate)
        [ fromJust (P.evalConstant estimate (M.findWithDefault 0 v derivs))
        | derivs <- derivss ne
        , v <- M.keys estimate
        ]

zero :: Ord var => Double -> [Polynomial Double var] -> Map var Double -> Map var Double
zero eps obj = go where
    ne = createEnvironment obj
    updateSize new 0 = new
    updateSize new old = abs ((new-old)/old)

    go estimate = if all (<eps) $ M.intersectionWith updateSize estimate estimate'
        then estimate'
        else go estimate'
        where estimate' = zeroStep ne estimate

fixedPoint :: Ord var => Double -> Map var (Polynomial Double var) -> Map var Double
fixedPoint eps eqns = zero eps
    [p - P.variable v | (v, p) <- M.toList eqns]
    (P.constantTerm <$> eqns)
