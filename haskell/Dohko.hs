module Dohko where
import System.Random
data ApplicationDescriptor = ApplicationDescriptor {applications :: [Application]} deriving (Show, Eq, Ord) 
data Application = Application {appInstrument :: String, appCommand :: String, appArgument:: String} deriving (Show, Eq, Ord) 
data ExecutionResult = ExecutionResult {resDependentVariableName :: String, resTreatmentName :: String, resObjectName :: String, value :: IO Float} 

dohko :: ApplicationDescriptor -> [IO Float]
dohko applicationDescriptor =map execute (applications applicationDescriptor) 
execute :: Application -> IO Float
execute application = getStdRandom (randomR (1,1000)) 
