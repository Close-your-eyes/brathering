library(brathering)
library(magrittr)
ncbi_data <- webscrape_ncbi(accession = "NC_001348.1") # https://www.ncbi.nlm.nih.gov/nuccore/NC_001348.1/
feat <- ncbi_data[["features"]]
feat_sub <- dplyr::filter(feat, Feature == "CDS", Subfeature == "locus_tag")
seqs <- get_seqs_from_feature_df(feature_df = feat_sub, origin = ncbi_data$origin, concat = T)


# how to speed up match/mismatch/gap assigment in plotting?
# check with examples that actually insert gaps in subject.
alignment <- MultiplePairwiseAlignmentsToOneSubject(subject = ncbi_data$origin,
                                                    patterns = Biostrings::DNAStringSet(unlist(seqs[1:5])),
                                                    rm_indel_inducing_pattern = F,
                                                    type = "global",
                                                    seq_type = "NT")

test_pattern <- strsplit(seqs[[1]], "")[[1]]
# insert 2 nt to trigger gap in subject
test_pattern <- c(test_pattern[1:108], "TA", test_pattern[109:length(test_pattern)])
# two mismatches
test_pattern[20] <- "G"
test_pattern[21] <- "G"
test_pattern <- paste(test_pattern, collapse = "")

test_pattern2 <- strsplit(seqs[[2]], "")[[1]]
# insert 2 nt to trigger gap in subject
test_pattern2 <- c(test_pattern2[1:108], "TA", test_pattern2[109:length(test_pattern2)])
# two mismatches
test_pattern2[20] <- "G"
test_pattern2[21] <- "G"
test_pattern2 <- paste(test_pattern2, collapse = "")

test_subject <- substr(ncbi_data$origin, 1, 1000)
test_subject <- strsplit(test_subject, "")[[1]]
# insert 2 nt to trigger gap in subject
test_subject <- c(test_subject[1:350], "GC", test_subject[351:length(test_subject)])
test_subject[410] <- "C"
test_subject[411] <- "A"
test_subject <- paste(test_subject, collapse = "")

alignment <- MultiplePairwiseAlignmentsToOneSubject(subject = test_subject,
                                                    patterns = c(test_pattern, test_pattern2),
                                                    rm_indel_inducing_pattern = F,
                                                    type = "local")


alignment[["match.plot"]] + ggplot2::xlim(c(alignment[["min.max.subject.position"]][1] - 100,
                                            alignment[["min.max.subject.position"]][2] + 500))

algn <- Biostrings::pairwiseAlignment(subject = Biostrings::DNAStringSet(ncbi_data$origin),
                                      pattern = Biostrings::DNAStringSet(seqs[2]),
                                      type = "global-local")

# this is slow for large algnt
igsc::printPairwiseAlignment(algn, "/Users/vonskopnik/Documents/R_packages/brathering/inst/extdata/test.txt")



