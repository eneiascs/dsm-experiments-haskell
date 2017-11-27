module RScript where
import Dohko
data RScript = RScript {analysisTests :: [AnalysisTest]} 
data AnalysisTest = AnalysisTest {analysisFunction :: [ExecutionResult]->[ExecutionResult]->TestResult, argument1 :: Argument, argument2::Argument} 
data Argument = Argument {argDvName :: String, argTreatmentName :: String, argObjectName ::String} deriving (Show, Eq, Ord)
data TestResult = TestResult {testResult :: String}  deriving (Show, Eq, Ord) 


analyze :: [ExecutionResult] -> RScript -> [TestResult]
analyze executionResults rscript = map ((\results test ->(analysisFunction test) (filterResults results (argument1 test)) (filterResults results (argument2 test) )) executionResults) (analysisTests rscript)

filterResults :: [ExecutionResult] -> Argument -> [ExecutionResult]
filterResults results (Argument dependentVariableName treatmentName objectName) = ([ result | result <- results, (resDependentVariableName result) == dependentVariableName && (resTreatmentName result)==treatmentName && (resObjectName result)==objectName])

