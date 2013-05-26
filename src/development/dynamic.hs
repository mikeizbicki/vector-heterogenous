import Data.Vector.Heterogenous.HList
import Data.Csv
import Data.Dynamic

line="1,2,3,test,funky,5"

type Line = HList '[Double,Double,Double,String,String,Double]