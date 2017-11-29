module Analysis where
import ExecutionResult
data AnalysisScript = AnalysisScript {hypothesesTests :: [HypothesisTest]}
data HypothesisTest = HypothesisTest {hypName::String, analysisTests :: [AnalysisTest]} 
data AnalysisTest = AnalysisTest {analysisFunction :: [ExecutionResult]->[ExecutionResult]->AnalysisResult, argument1 :: Argument, argument2::Argument} 
data Argument = Argument {argDvName :: String, argTreatmentName :: String, argObjectName ::String} 
data HypothesisResult = HypothesisResult {hrHypothesisName::String, testResults::[TestResult]}  deriving (Show, Eq, Ord) 
data TestResult = TestResult {trObjectName::String, analysisResult :: AnalysisResult}   deriving (Show, Eq, Ord) 
data AnalysisResult = AnalysisResult {result :: String}  deriving (Show, Eq, Ord) 

analyze :: [ExecutionResult] -> AnalysisScript -> [HypothesisResult]
analyze results (AnalysisScript hypothesesTests) = map (analyzeHypothesis results) hypothesesTests

analyzeHypothesis :: [ExecutionResult] -> HypothesisTest -> HypothesisResult
analyzeHypothesis executionResults ht = HypothesisResult (hypName ht) (map (applyAnalysisFunction executionResults) (analysisTests ht))

applyAnalysisFunction :: [ExecutionResult] -> AnalysisTest -> TestResult
applyAnalysisFunction results test = TestResult (argObjectName (argument1 test)) ((analysisFunction test) (filterResults results (argument1 test)) (filterResults results (argument2 test) )) 

filterResults :: [ExecutionResult] -> Argument -> [ExecutionResult]
filterResults results (Argument dependentVariableName treatmentName objectName) = ([ result | result <- results, (resDependentVariableName result) == dependentVariableName && (resTreatmentName result)==treatmentName && (resObjectName result)==objectName])

