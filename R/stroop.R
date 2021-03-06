#' Stroop Dataset from Von Bastian et al. (2015)
#'
#' The dataset from on Von Bastian et al. (2015) on the Stroop task.
#'
#' @format A \code{data.frame} of 11245 rows and 7 columns
#' \describe{
#'   \item{ID}{Factor giving the participant ID: 1-121}
#'   \item{congruency}{Factor giving one of the two conditions: congruent and incongruent}
#'   \item{rtMS}{Numeric values of the response times in milliseconds}
#'   \item{accuracy}{Numeric values giving the accuracy of the response, here only 1 = correct}
#'   \item{cond}{Factor giving the condition: 1 = congruent, 2 = incongruent}
#'   \item{trial}{Numeric values indicating the trial number of the participant}
#'   \item{rtS}{Numeric values giving the response time in seconds}
#'   }
#'
#' @source Von Bastian, C. C., Souza, A. S., & Gade, M. (2016). No evidence for
#'   bilingual cognitive advantages: A test of four hypotheses. Journal of
#'   Experimental Psychology: General, 145(2), 246., retrieved from
#'   \url{https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/contexteffects/FlankerStroopSimon/LEF_stroop.csv}
"stroop"
