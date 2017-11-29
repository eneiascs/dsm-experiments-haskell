
import Experiment
import ExecutionResult
import Infrastructure
import Analysis 


main = do
  
  let featureFamily= Treatment "featureFamily" "FEATURE_FAMILY" 
  let featureProduct= Treatment "featureProduct" "FEATURE_PRODUCT"
  let product= Treatment "product" "PRODUCT"  
  let time = DependentVariable "time" "timeInstrument"
  let memory = DependentVariable "memory" "memoryInstrument"
  let rh1 = ResearchHypothesis "RH1" time featureFamily "=" featureProduct
  let rh2 = ResearchHypothesis "RH2" time featureFamily "=" product
  let bsn = ExperimentalObject "bsn" "bsn"
  let email = ExperimentalObject "email" "email"
  let lift = ExperimentalObject "lift" "lift"
  let intercloud = ExperimentalObject "intercloud" "intercloud"
  let minepump = ExperimentalObject "minepump" "minepump"
  let tankwar = ExperimentalObject "tankwar" "tankwar"
  let design = ExperimentalDesign 2 cartesianProductDesign 
  let exp = Experiment [rh1,rh2] design [featureFamily,featureProduct] [bsn] [time] 
  let executionScript = compileExecutionScript exp
  let analysisScript = generateAnalysisScript exp
  let executionResults = execute executionScript
  let experimentResults= experiment exp
  print executionScript
  print experimentResults
  
 
