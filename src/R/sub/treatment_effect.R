#' Calculation for treatment effect using IPW.
#' 
#' Snipped from internet.
#' (https://livefreeordichotomize.com/2019/01/17/understanding-propensity-score-weighting/
#' 
#' @parameter treatment A numeric vector of treatment indicator.
#' @parameter outcome A numeric vector of treatment indicator.
#' @parameter weight A numeric vector of weight.
#' 
#' @export

treatment_effect <- 
  function(treatment, outcome, weight) {
    dat.cc <- 
      data.frame(treatment=treatment,outcome=outcome,weight=weight) 
    dat.cc <- 
      dat.cc[complete.cases(dat.cc),]
    treatment.effect <-
      (sum(
      dat.cc$treatment * dat.cc$outcome * dat.cc$weight) / sum(dat.cc$treatment * dat.cc$weight)) +
      (sum(
        (1 - dat.cc$treatment) * dat.cc$outcome * dat.cc$weight) / sum((1 - dat.cc$treatment) * dat.cc$weight)
      )
    return(treatment.effect)
  }
