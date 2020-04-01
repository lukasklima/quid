#' Stroop Dataset from Rouder
#'
#' The dataset from Rouder (20xx) on the Stroop task.
#'
#' @format A \code{data.frame} of 11245 rows and 7 columns
#' \describe{
#'  \item{ID}{Numeric values giving the participant ID, 1-121}
#'  \item{congruency}{Factor giving one of the two conditions: congruent and incongruent}
#'  \item{RT}{Numeric values of the response times in milliseconds}
#'  \item{accuracy}{Numeric values giving the accuracy of the response, here only 1 = correct}
#'  \item{cond}{Numeric values giving the condition, 1 = congruent, 2 = incongruent}
#'  \item{trial}{Numeric values indicating the trial number of the participant}
#'  \item{rt}{Numeric values giving the response time in seconds}
#'  }
#'
#' @source Data from Rouder (20xx), retrieved from https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/contexteffects/FlankerStroopSimon/LEF_stroop.csv
"stroop"