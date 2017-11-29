module Infrastructure where
import System.Random
data ExecutionScript = ExecutionScript {applications :: [Application]} deriving (Show, Eq, Ord) 
data Application = Application {appInstrument :: String, appCommand :: String, appArgument:: String} deriving (Show, Eq, Ord) 

execute :: ExecutionScript -> [IO Float]
execute applicationDescriptor =map executeApplication (applications applicationDescriptor) 
executeApplication :: Application -> IO Float
executeApplication application = getStdRandom (randomR (1,1000)) 
