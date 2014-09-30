#' @title Write Spip output to a file
#'
#' @description Write the output from a Spip run to a file
#'
#' @param output a string vector with the output from Spip
#'   (\code{output} element of the return value from \code{\link{spipRunSim}})
#' @param file filename
#'
#' @examples
#' library(PKDtroutR)
#' d = subset(trout, trout$field_trip_number == "september" &
#'   trout$full_origin == "vainupea")
#' microsats = c( "Ssosl438_1", "Ssosl438_2", "Ssosl311_1", "Ssosl311_2",
#'   "Str15inraP_1", "Str15inraP_2", "LG.14_1_1", "LG.14_1_2", "Str543inraP_1",
#'   "Str543inraP_2", "Ssa197_1", "Ssa197_2", "LG.15_1_1", "LG.15_1_2",
#'   "Strutta.58_1", "Strutta.58_2", "Str60inra_1", "Str60inra_2", "Str73inra_1",
#'   "Str73inra_2", "Ssosl417_1", "Ssosl417_2", "Str85inraP_1", "Str85inraP_2",
#'   "LG.10_2_1", "LG.10_2_2", "Bs131_1", "Bs131_2", "Ssa407_1", "Ssa407_2" )
#' ids = d$fish_global_id
#' genotypes = d[, microsats]
#' 
#' a = spipRunSim(ref_genotypes = genotypes,
#'   max_age = 4,
#'   number_of_years = 50,
#'   survival_probs = c(0.2, 0.3, 0.5, 0.2),
#'   fem_asrf = c(0, 0.2, 1, 0.8),
#'   fem_prob_repro = c(0, 0.2, 0.5, 0.3),
#'   initial_females = c(500, 300, 40, 20),
#'   male_asrp = c(0, 0.3, 1, 1),
#'   male_prob_repro = c(0, 0.2, 0.5, 0.3),
#'   initial_males = c(500, 300, 40, 20),
#'   fixed_cohort_size = T,
#'   cohort_size = 500,
#'   discard_parents = 4,
#'   gtyps_for_all = 50)
#'
#' spipWriteOutput(output = a$output, file = "spip.results")
#'
#' @export
#'

spipWriteOutput = function(output, file) {
  f = file(description = file, open = "w")
  cat(output, file = f, sep = "\n")
  cat("\n", file = f)
  close(f)
}
