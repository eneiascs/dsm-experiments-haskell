module ExecutionResult where
data ExecutionResult = ExecutionResult {resDependentVariableName :: String, resTreatmentName :: String, resObjectName :: String, value :: IO Float} 

