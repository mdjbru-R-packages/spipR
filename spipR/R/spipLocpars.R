#' @title Write a locus information file for Spip
#'
#' @description Write a locus information file for Spip. Most of the
#'   description come from \code{spip_man_page.pdf}. The format of this
#'   allele-frequency information file is simple. It includes only integers
#'   (or comments enclosed by a pair of & characters). The first integer is
#'   the number of loci in the file. Then for each locus you give the number
#'   of alleles at that locus followed by the allele frequencies of each
#'   allele at the locus. All these things should be sepa- rated by
#'   whitespace. No punctuation! If the allele frequencies at a locus do not
#'   sum to one they will be rescaled so that the do sum to one. If the number
#'   of allele frequencies listed does not match the number of alleles given
#'   for the locus then the program may fail without warning or give
#'   otherwise unexpected results.
#'   \code{NA} in the genotype input are not taken into account.
#'
#' @param genotypes a data frame with columns working by pairs. Every
#'   successive pair of columns represents the alleles for one locus.
#'   Rows are individuals.
#' @param file file name used to save to locus information file
#' @param comment optional comment string
#'
#' @return A list with the parameters used
#'
#' @examples
#' n_ind = 10
#' locus1_1 = sample(1:5, n_ind, replace = T)
#' locus1_2 = sample(1:5, n_ind, replace = T)
#' locus2_1 = sample(1:8, n_ind, replace = T)
#' locus2_2 = sample(1:8, n_ind, replace = T)
#' locus3_1 = sample(c(1:3, NA), n_ind, replace = T)
#' locus3_2 = sample(1:3, n_ind, replace = T)
#' genotypes = data.frame(locus1_1, locus1_2,
#'   locus2_1, locus2_2,
#'   locus3_1, locus3_2)
#' a = spipLocpars(genotypes = genotypes, file = "toto.locpars")
#'
#' @export
#'

spipLocpars = function(genotypes,
  file,
  comment = "Generated using spipR") {

  stopifnot(ncol(genotypes) %% 2 == 0)
  loci = list()
  n = ncol(genotypes) / 2
  for (i in 1:n) {
    g = unlist(genotypes[, c(2*i-1, 2*i)])
    g = na.omit(g)
    g_info = as.numeric(table(g))
    loci[[i]] = g_info
  }
  o = paste0("& ", comment, " &\n")
  o = paste0(o, as.character(n), "\n")
  add_o = function(locus) {
    n_alleles = length(locus)
    counts = paste(as.character(locus), collapse = " ")
    l = paste(as.character(n_alleles), counts, sep = " ")
    paste0(o, l, "\n")
  }
  for (i in 1:length(loci)) {
    locus = loci[[i]]
    o = add_o(locus)
  }
  f = file(description = file, open = "w")
  cat(o, file = f)
  close(f)

  # data summary
  m = list()
  m[["genotypes"]] = genotypes
  m[["file"]] = file
  m[["comment"]] = comment
  m[["locpars"]] = o
  
  # return
  return(m)
}
