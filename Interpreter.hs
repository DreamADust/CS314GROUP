import Data.Function
import Data.List

module Lambdas
( Lambdas(Triangle)) where


type LET = ... ;
type Vector = ... ;
data Triangle
  = Lam Vector
  | Lam Triangle Vector Triangle
  | Lam Triangle Triangle
  | 

data L =  LET LL Triangle

data LL = Vector Triangle
        | Vector Triangle LL



