#' @title Run a pedigree simulation with Spip
#'
#' @description Run a pedigree simulation with Spip. For details about the
#'   parameters not explained here see mainly \code{\link{spipDemog}} and
#'   also \code{\link{spipGtyp}}.
#'
#' @param ref_genotypes Used for allele frequencies. A data frame with
#'   columns working by pairs. Every successive pair of columns represents the
#'   alleles for one locus. Rows are individuals.
#'
#' @return The input parameters and the output from Spip
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
#' # generate many simulations
#' n = as.list(1:100)
#' sims = lapply(n, function(x) {
#'   spipRunSim(ref_genotypes = genotypes,
#'              max_age = 4,
#'              number_of_years = 50,
#'              survival_probs = c(0.2, 0.3, 0.5, 0.2),
#'              fem_asrf = c(0, 0.2, 1, 0.8),
#'              fem_prob_repro = c(0, 0.2, 0.5, 0.3),
#'              initial_females = c(500, 300, 40, 20),
#'              male_asrp = c(0, 0.3, 1, 1),
#'              male_prob_repro = c(0, 0.2, 0.5, 0.3),
#'              initial_males = c(500, 300, 40, 20),
#'              fixed_cohort_size = T,
#'              cohort_size = 500,
#'              discard_parents = 4,
#'              gtyps_for_all = 50)
#' })
#' # check their random seeds
#' seeds = lapply(sims, function(x) {
#'    c = grepl("RAND", x$output)
#'  return(x$output[c])
#' })
#'
#' # This shows that the seeds are the same for runs performed during a single
#' # second.
#'
#' @export
#'

spipRunSim = function(ref_genotypes,
  max_age,
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
  discard_parents,
  gtyps_for_all) {

  library(uuid)
  f = paste0("spipR-tempfile-", UUIDgenerate())
  
  # make locpars file
  locpars = spipLocpars(genotypes = ref_genotypes,
              file = paste0(f, ".locpars"))
  
  # make demog file
  demog = spipDemog(max_age = max_age,
            number_of_years = number_of_years,
            survival_probs = survival_probs,
            fem_asrf = fem_asrf,
            fem_prob_repro = fem_prob_repro,
            initial_females = initial_females,
            male_asrp = male_asrp,
            male_prob_repro = male_prob_repro,
            initial_males = initial_males,
            fixed_cohort_size = fixed_cohort_size,
            cohort_size = cohort_size,
            file = paste0(f, ".demog"))
  
  # make gtyp file
  gtyp = spipGtyp(locus_file = paste0(f, ".locpars"),
           discard_parents = discard_parents,
           gtyps_for_all = gtyps_for_all,
           file = paste0(f, ".gtyp"))
  # run
  #warning("\"spip_seeds\" was removed (if any file like this existed). This means Spip will use the clock to initialize the seeds. This might cause problems if Spip is run simultaneously or in a very short time (similar seeds between simulations).")
  c = paste("spip", "--command-file", paste0(f, ".demog"),
    "--command-file", paste0(f, ".gtyp"))
  print(c)
  output = system(c, intern = TRUE)

  # check the seeds
  if (sum(grepl("Using clock to set RNG", output)) == 1) {
    # the clock was used, get those values for the next one
    seeds = output[grepl("Using clock to set RNG", output)]
    seeds = strsplit(seeds, split = "Using clock to set RNG: ")[[1]][2]
    seeds = as.numeric(strsplit(seeds, " ")[[1]])
    seeds = seeds + 1
    seeds[1] = seeds[1] + floor((10 ** sample(2:6, 1)) * runif(1))
    seeds = as.character(seeds)
    seeds = paste(seeds, collapse = " ")
    fseeds = file("spip_seeds", "w")
    cat(seeds, file = fseeds)
    close(fseeds)
    #warning(paste0("\"spip_seeds\" overwritten with ", seeds))
  } else {
    stopifnot(sum(grepl("Using contents of spip_seeds to set RNG",
                        output)) == 1)
    # the spip_seeds file was used
    seeds = output[grepl("Using contents of spip_seeds to set RNG", output)]
    seeds = strsplit(seeds,
      split = "Using contents of spip_seeds to set RNG: ")[[1]][2]
    seeds = as.numeric(strsplit(seeds, " ")[[1]])
    seeds = seeds + 1
    seeds[1] = seeds[1] + floor((10 ** sample(2:6, 1)) * runif(1))
    seeds = as.character(seeds)
    seeds = paste(seeds, collapse = " ")
    fseeds = file("spip_seeds", "w")
    cat(seeds, file = fseeds)
    close(fseeds)
    #warning(paste0("\"spip_seeds\" overwritten with ", seeds))    
  }
  
  # delete files
  for (e in c(".locpars", ".demog", ".gtyp")) {
    file.remove(paste0(f, e))
  }
  
  # return result
  results = list()
  results[["input"]] = c(locpars, demog, gtyp)
  results[["output"]] = output
  return(results)
}
