import Control.DeepSeq
import qualified Data.Vector as V
import Data.Vector.Heterogenous


vh size = V.generate size $ \i -> tup (ShowBox ()) $ (i::Int):::(i::Int):::(i::Int):::(i::Int):::(i::Int):::HNil
vv size = V.generate size $ \i -> V.replicate 5 i

main = do
    print $ vh 10000
--     print vh