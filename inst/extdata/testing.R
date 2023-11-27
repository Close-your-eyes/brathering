library(brathering)
ncbi_data <- webscrape_ncbi(accession = "NC_001348.1") # https://www.ncbi.nlm.nih.gov/nuccore/NC_001348.1/
feat <- ncbi_data[["features"]]
feat_sub <- feat %>% dplyr::filter(Feature == "CDS") %>% dplyr::filter(Subfeature == "locus_tag")
seqs <- get_seqs_from_feature_df(feature_df = feat_sub, origin = ncbi_data$origin, concat = T)


# how to speed up match/mismatch/gap assigment in plotting?
# check with examples that actually insert gaps in subject.
alignment <- MultiplePairwiseAlignmentsToOneSubject(subject = ncbi_data$origin,
                                                    patterns = Biostrings::DNAStringSet(unlist(seqs[1:5])),
                                                    rm_indel_inducing_pattern = F,
                                                    type = "global")

tt <- strsplit(seqs[[1]], "")[[1]]
tt <- c(tt[1:108], "TA", tt[109:length(tt)])
tt <- paste(tt, collapse = "")
alignment <- MultiplePairwiseAlignmentsToOneSubject(subject = ncbi_data$origin,
                                                    patterns = tt,
                                                    rm_indel_inducing_pattern = F,
                                                    type = "local")


alignment[["match.plot"]] + ggplot2::xlim(c(alignment[["min.max.subject.position"]][1] - 100,
                                            alignment[["min.max.subject.position"]][2] + 500))

algn <- Biostrings::pairwiseAlignment(subject = Biostrings::DNAStringSet(ncbi_data$origin),
                                      pattern = Biostrings::DNAStringSet(seqs[2]),
                                      type = "global-local")

# this is slow for large algnt
igsc::printPairwiseAlignment(algn, "/Users/vonskopnik/Documents/R_packages/brathering/inst/extdata/test.txt")



