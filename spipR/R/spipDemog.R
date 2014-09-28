#' @title Write a demography file for Spip
#'
#' @description Write a demography input file for Spip. Most of the parameter
#'   descriptions come from \code{spip_man_page.pdf}.
#'
#' @param max_age (-A) This sets the maximum age that any individual in
#'   the population can reach. All organisms are programmed to have died with
#'   probability one before they exceed this age. Integer. Required.
#' @param number_of_years (-T) Sets the number of years to carry out in
#'   the simulations. The years for which individuals "exist" in the
#'   simulations are from year 0 to year T + MaxAge - 1 inclusive. The
#'   first simulated offspring cohort is born at time MaxAge. Integer.
#' @param survival_probs (-s) [R1][R2] ...  age-specific probability of
#'   survival. R1 is the probabiilty of of surviving from age 0 to age 1. R2
#'   is the probability of surviving from age 1 to age 2. There should be
#'   MaxAge of these R arguments.  The last of them is the probability of
#'   surviving from age MaxAge-1 to age MaxAge. This option is required unless
#'   sex-specific survival rates are set using the fem-surv-probs and the
#'   male-surv- probs options. Vector of floats.
#' @param fem_asrf (-f) [R1][R2] ...  age-specific relative
#'   fecundities. R1 is the relative fecundity of a one-year old female. R2 is
#'   the same for a 2 year-old female. There should be MaxAge of these R
#'   arguments. The last one is the relative fecundity of MaxAge-year-old
#'   females. It is assumed that 0-year olds do not reproduce.  These numbers
#'   need not sum to one. All that matters is their _relative_
#'   sizes. Required. Vector of floats.
#' @param fem_prob_repro [R1][R2] ...  R1 is the absolute probability that
#'   a one-year old female will reproduce. R2 is the same for a 2 year-old
#'   female. There should be MaxAge of these R arguments. The last one is the
#'   probability that a MaxAge-year-old female reproduces. It is assumed that
#'   0-year olds do not reproduce. These numbers need not sum to one. They
#'   must be between zero and 1. They are not relative values.
#'   Required. Vector of floats.
#' @param initial_females [J0][J1] ...  Initial number of females of
#'   different ages. J0 is the number of newborn females at time MaxAge; J1 is
#'   the number of one-year-old females still alive at time MaxAge and so
#'   forth. There should be MaxAge such Js because the first thing to happen
#'   is an episode of death going from time MaxAge to time MaxAge+1; thus any
#'   MaxAge-year-olds would die before reproducing anyway. The last J is the
#'   number of MaxAge-1-year-old females at the outset (i.e. at time
#'   MaxAge). Vector of integers.
#' @param male_asrp [R1][R2] ...  age-specific relative male reproductive
#'   potential. R1 is the relative reproductive potential of a one-year old
#'   male. R2 is the same for a 2 year-old male. There should be MaxAge of
#'   these R arg#uments. The last one is the relative reproductive potential
#'   of MaxAge-year-old males. It is assumed that 0-year olds do not
#'   reproduce. These numbers need not sum to one. All that matters is their
#'   _relative_ sizes. Required. Vector of floats.
#' @param male_prob_repro [R1][R2] ...  R1 is the absolute probability
#'   that a one-year old male will reproduce. R2 is the same for a 2 year- old
#'   male. There should be MaxAge of these R arguments. The last one is the
#'   probability that a MaxAge-year-old male reproduces. It is assumed that
#'   0-year olds do not reproduce. These num- bers need not sum to one. They
#'   must be between zero and 1. They are not relative values.
#'   Required. Vector of floats.
#' @param initial_males [J0][J1] ...  Initial number of males of different
#'   ages. J0 is the number of newborn males at time MaxAge; J1 is the number
#'   of one-year-old males still alive at time MaxAge and so forth. There
#'   should be Max- Age such Js because the first thing to happen is an
#'   episode of death going from time MaxAge to time MaxAge+1; thus any
#'   MaxAge-year-olds would die before reproducing anyway. The last J is the
#'   number of MaxAge-1-year-old males at the outset (i.e. at time
#'   MaxAge). Vector of integers.
#' @param fixed_cohort_size Boolean. Default \code{TRUE}, if \code{FALSE}
#'   then random cohort size is used.
#' @param cohort_size (\code{const} option in Spip) [J] --- this makes the
#'   cohort size (or at least the _expected_ cohort size (if
#'   \code{fixed_cohort_size} is set to \code{FALSE})) J every year regardless
#'   of how large the parental population is. Integer.
#' @param file string, file name used to save the demography file.
#' @param comment optional comment string
#'
#' @return A list with the specified parameters
#'
#' @examples
#' d = spipDemog(max_age = 4,
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
#'   file = "toto.demog")
#' 
#' @export
#'

spipDemog = function(max_age,
  number_of_years,
  survival_probs,
  fem_asrf,
  fem_prob_repro,
  initial_females,
  male_asrp,
  male_prob_repro,
  initial_males,
  fixed_cohort_size = TRUE,
  cohort_size,
  file,
  comment = "Generated using spipR") {

  # prepare the file
  o = paste0("& ", comment, " &\n")
  add_o = function(param, content = NULL) {
    c = paste(as.character(content), collapse = " ")
    l = paste(param, c, sep = " ")
    paste0(o, l, "\n")
  }
  o = add_o("-A", max_age)
  o = add_o("-T", number_of_years)
  o = add_o("-s", survival_probs)
  o = add_o("-f", fem_asrf)
  o = add_o("--fem-prob-repro", fem_prob_repro)
  o = add_o("--initial-females", initial_females)
  o = add_o("-m", male_asrp)
  o = add_o("--male-prob-repro", male_prob_repro)
  o = add_o("--initial-males", initial_males)
  if (fixed_cohort_size) {
    o = add_o("--fixed-cohort-size")
  }
  o = add_o("--cohort-size const", cohort_size)
  f = file(description = file, open = "w")
  cat(o, file = f)
  close(f)

  # prepare the data summary
  m = list()
  m[["summary_table"]] = data.frame(age = 0:max_age,
     survival_probs = c(survival_probs, 0),
     female_asrf = c(0, fem_asrf),
     female_prob_repro = c(0, fem_prob_repro),
     initial_females_n = c(initial_females, 0),
     male_asrp = c(0, male_asrp),
     male_prob_repro = c(0, male_prob_repro),
     initial_males_n = c(initial_males, 0))
  m[["max_age"]] = max_age
  m[["number_of_years"]] = number_of_years
  m[["fixed_cohort_size"]] = fixed_cohort_size
  m[["cohort_size"]] = cohort_size
  m[["file"]] = file
  m[["comment"]] = comment
  m[["demog"]] = o
  
  # return
  return(m)
}
