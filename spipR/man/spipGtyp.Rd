% Generated by roxygen2 (4.0.1): do not edit by hand
\name{spipGtyp}
\alias{spipGtyp}
\title{Write a genotype file for Spip}
\usage{
spipGtyp(locus_file, discard_parents, gtyps_for_all, file,
  comment = "Generated using spipR")
}
\arguments{
\item{locus_file}{pathname to file with locus information}

\item{discard_parents}{[J] This option discards in the pedigree any
parents who were born before time J. If a parent (let us say a father)
born before time J produces an offspring then that child will be
considered to have an unkown father. (i.e. in the pedigree the child will
have a 0 for its father but not necessarily for its mother.) The pedigree
that would have been simulated without the discard-parents option gets
printed on the DISCARDED_PDGEE lines of output. There will be discarded
pedigree outputs until the cohort born at time J + MaxAge because after
that time it is not possible for a child to have a parent that was born
before time J. When spip simulates genetic data the paternal allele
founder is the child if the father was discarded. This option is not
terribly useful. Unfortunately the programs of the MORGAN package cannot
deal with pedigree with only one parent having a 0. It is possible to get
a similar effect by using the --discard-all option and running the
simulation a little longer. Integer.}

\item{gtyps_for_all}{[Range] This causes genotypes to be simulated for
all individuals born into cohorts at the times specified in
[Range]. [Range] must be a list of nonnegative numbers in increasing
order separated by commas and dashes as described below.  [Range] refers
to a string that gives a discontinous range of nonnegative integers. For
example: "1-5,7,9,10-15" specifies the integers 1 through 5, 7, 9, and 10
through 15. There can be no whitespace in the string specifying the
range, and the numbers must all be increasing. Also, the string cannot
start nor end with a comma or a dash. Finally, you should not use "-" to
denote two ranges without placing any com- mas in between. String or
compatible with string conversion.}

\item{file}{string, file name used to save the demography file.}

\item{comment}{optional comment string}
}
\value{
A list with the parameters
}
\description{
Write a genotype input file for Spip. Most of the parameter
  descriptions come from \code{spip_man_page.pdf}.
}
\examples{
a = spipGtyp(locus_file = "toto.locus_pars",
  discard_parents = 4,
  gtyps_for_all = "25,50")
}

