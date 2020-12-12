#' The dataset from Rouder et al. (2005) on the lexical task.
#'
#' This is the cleaned dataset from Rouder et al. (2005) on the lexical task.
#' Participants were presented with number stimuli ranging from 2 to 4 and from
#' 6 to 8 and had to indicate whether the stimuli number is bigger or smaller
#' than 5. The data cleaning that was performed was: removing two participants
#' who gave up, excluding trials with too fast and too slow responses, excluding
#' trials with wrong responses, excluding the first 20 trials of the first block,
#' excluding the first trial in every block.
#'
#' @format A \code{data.frame} of 17031 rows and 9 columns
#' \describe{
#'   \item{sub}{Factor with 52 levels giving the subject ID: 0-29, 35-42, 44-56, 58}
#'   \item{block}{Numeric values giving the block: 1-5}
#'   \item{trial}{Numeric values giving the number of the trial within a block}
#'   \item{stim}{Numeric values giving the type of stimulus: 0 = 2, 1 = 3, 2 = 4, 3 = 6, 4 = 7, 5 = 8}
#'   \item{resp}{Numeric values giving the participant's response: 1 = bigger, 2 = smaller}
#'   \item{rt}{Numeric values giving the response times in milliseconds}
#'   \item{error}{Numeric values indicating whether participant gave a wrong answer (all 0)}
#'   \item{side}{Factor with 2 levels indicating whether stimulus is below (1) or above (2) 5}
#'   \item{distance}{Factor with 3 levels indicating how far the stimulus number is away from 5}
#'   }
#' @source Rouder, J. N., Lu, J., Speckman, P., Sun, D., & Jiang, Y. (2005). A
#'   hierarchical model for estimating response time distributions. Psychonomic
#'   Bulletin & Review, 12(2), 195-223., retrieved from
#'   \url{https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/lexDec-dist5/ld5.all}
"ld5"
