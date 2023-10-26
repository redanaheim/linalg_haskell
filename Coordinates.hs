{-# Language TypeFamilies, AllowAmbiguousTypes #-}
import Field
import Vector hiding (main)
import Data.Kind
import Matrix hiding (main)
import qualified Data.List as Foldable

class (Field (Component a)) => Coordinates a where 
    type Component a :: Type

    length :: a -> Int
    idx :: a -> Int -> Maybe (Component a)

instance (Field a) => Coordinates [a] where 
    type Component [a] = a

    length x = Foldable.length x
    idx x i = x !? i
