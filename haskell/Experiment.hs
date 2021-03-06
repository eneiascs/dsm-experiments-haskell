module Experiment where
import Infrastructure
import Analysis
import ExecutionResult

data Experiment = Experiment {researchHypotheses :: [ResearchHypothesis], design :: ExperimentalDesign, treatments:: [Treatment], objects:: [ExperimentalObject], dependentVariables :: [DependentVariable]} 
data ResearchHypothesis = ResearchHypothesis {hypothesisName :: String, dependentVariable :: DependentVariable, treatment1 :: Treatment,
operator::String, treatment2 :: Treatment} deriving (Show, Eq, Ord) 
data DependentVariable = DependentVariable {dvName :: String, instrumentCommand :: String} deriving (Show, Eq, Ord) 
data ExperimentalDesign =  ExperimentalDesign {runs :: Int, designFunction :: [Treatment]->[ExperimentalObject] ->[(Treatment,ExperimentalObject)] } 
data Treatment = Treatment {treatmentName :: String, treatmentCommand :: String} deriving (Show, Eq, Ord) 
data ExperimentalObject = ExperimentalObject {objectName :: String, argument:: String} deriving (Show, Eq, Ord)

experiment :: Experiment -> [HypothesisResult]
experiment exp= analyze  executionResults (generateAnalysisScript exp) 
  where executionResults = map createExecutionResult (zip (generateListOfExecutions exp) (execute (compileExecutionScript exp))) 

compileExecutionScript :: Experiment -> ExecutionScript
compileExecutionScript experiment = ExecutionScript (map compileApplication (generateListOfExecutions experiment))  

compileApplication ::(Treatment,ExperimentalObject, DependentVariable) -> Application
compileApplication (treatment,object, depVariable) = Application (instrumentCommand depVariable) (treatmentCommand treatment) (argument object)

generateListOfExecutions :: Experiment -> [(Treatment,ExperimentalObject,DependentVariable)]
generateListOfExecutions experiment = concatMap (replicate (runs (design experiment))) (removeDuplicates treatmentApplicationDependentVariable)
  where treatmentApplicationDependentVariable = (concatMap (applyDesign (design experiment) (objects experiment)) (researchHypotheses experiment))

applyDesign :: ExperimentalDesign -> [ExperimentalObject]-> ResearchHypothesis -> [(Treatment,ExperimentalObject,DependentVariable)]
applyDesign design objects hypothesis = map ((\a (b,c) -> (b,c,a)) (dependentVariable hypothesis)) treatmentApplication
  where treatmentApplication = (designFunction design) [treatment1 hypothesis, treatment2 hypothesis] objects
 
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) | x `elem` xs   = removeDuplicates xs
                        | otherwise     = x : removeDuplicates xs


createExecutionResult :: ((Treatment,ExperimentalObject, DependentVariable),IO Float)->ExecutionResult
createExecutionResult ((treat, obj, depVariable), value) = ExecutionResult (dvName depVariable) (treatmentName treat) (objectName obj) value

cartesianProductDesign:: [Treatment]->[ExperimentalObject]->[(Treatment,ExperimentalObject)]
cartesianProductDesign treatments objects =[(treatment,object) | treatment <- treatments, object<-objects]

generateAnalysisScript :: Experiment -> AnalysisScript
generateAnalysisScript experiment = AnalysisScript (map (generateHypothesisTests (objects experiment) (design experiment)) (researchHypotheses experiment)) 

generateHypothesisTests :: [ExperimentalObject] -> ExperimentalDesign -> ResearchHypothesis -> HypothesisTest 
generateHypothesisTests objects design hypothesis = HypothesisTest (hypothesisName hypothesis) (map (createAnalysisTest hypothesis) commonObjects)
  where treatmentApplication = (designFunction design) [treatment1 hypothesis, treatment2 hypothesis] objects
        commonObjects =[object | object<-objects, elem ((treatment1 hypothesis),object) treatmentApplication && elem ((treatment2 hypothesis),object) treatmentApplication]
  
createAnalysisTest :: ResearchHypothesis -> ExperimentalObject -> AnalysisTest
createAnalysisTest  rh object = AnalysisTest (suitableFunction rh) argument1 argument2 
  where argument1 = Argument (dvName (dependentVariable rh)) (treatmentName (treatment1 rh)) (objectName object)
        argument2 = Argument (dvName (dependentVariable rh)) (treatmentName (treatment2 rh)) (objectName object)

suitableFunction :: ResearchHypothesis -> ([ExecutionResult]->[ExecutionResult]->AnalysisResult)
suitableFunction rh = wilcoxTest

wilcoxTest :: [ExecutionResult]->[ExecutionResult]->AnalysisResult
wilcoxTest sample1 sample2 = AnalysisResult "some result" 

