import Control.DeepSeq
import qualified Data.Vector as V
import Data.Vector.Heterogenous


ve size = V.generate size $ \i -> vec (ShowBox ()) $ (i::Int):::(i::Int):::(i::Int):::(i::Int):::(i::Int):::HNil
vt size = V.generate size $ \i -> tup $ (i::Int):::(i::Int):::(i::Int):::(i::Int):::(i::Int):::HNil
vv size = V.generate size $ \i -> V.fromList [i,i,i,i,i]

main = do
    print $ fmap getvec $ ve 10000
--     print vh