#BiocManager::install("Rsubread")
library(brathering)
library(magrittr)
ncbi_data <- webscrape_ncbi(accession = "NC_001348.1") # https://www.ncbi.nlm.nih.gov/nuccore/NC_001348.1/
feat <- ncbi_data[["features"]]
feat_sub <- dplyr::filter(feat, Feature == "CDS", Subfeature == "locus_tag")
seqs <- get_seqs_from_feature_df(feature_df = feat_sub, origin = ncbi_data$origin, concat = T)

#?Rsubread::align()
#library(Rsubread)
# Build an index for the artificial sequence included in file 'reference.fa'.
ref <- system.file("extdata","reference.fa",package="Rsubread")
ref_import <- igsc::read_fasta(ref)
Rsubread::buildindex(basename="./reference_index",reference=ref) # this save index files to getwd()
file.info(list.files(dirname(ref), full.names = T))

# align a sample read dataset ('reads.txt') to the sample reference
reads <- system.file("extdata","reads.txt.gz",package="Rsubread")

align.stat <- align(index = "./reference_index", readfile1 = reads,
                    output_file = "./Rsubread_alignment.BAM", phredOffset = 64)

align.stat

alignment <- MultiplePairwiseAlignmentsToOneSubject(subject = substr(ncbi_data$origin, 1, 5000),
                                                    patterns = list(Biostrings::DNAStringSet(unlist(seqs[c(1:4,2,2,4)]))),
                                                    rm_indel_inducing_pattern = F,
                                                    type = "local",
                                                    seq_type = "NT",
                                                    algnmt_plot_args = list(add_length_suffix = T,
                                                                            plot.pattern.names = T,
                                                                            plot.pattern.names_fun = ggplot2::geom_text,
                                                                            group_on_yaxis = T))

alignment <- MultiplePairwiseAlignmentsToOneSubject(subject = substr(ncbi_data$origin, 1, 15000),
                                                    patterns = list(Biostrings::DNAStringSet(unlist(seqs[c(1:5)])),
                                                                    Biostrings::DNAStringSet(unlist(seqs[5:9])),
                                                                    Biostrings::DNAStringSet(unlist(seqs[c(5:8)]))),
                                                    rm_indel_inducing_pattern = F,
                                                    type = "local",
                                                    seq_type = "NT",
                                                    algnmt_plot_args = list(add_length_suffix = T,
                                                                            plot.pattern.names = T,
                                                                            plot.pattern.names_fun = ggplot2::geom_text,
                                                                            group_on_yaxis = T,
                                                                            min_gap = 400))
alignment <- MultiplePairwiseAlignmentsToOneSubject(subject = substr(ncbi_data$origin, 1, 15000),
                                                    patterns = seqs[c(1:4, 5:7, 5:8, 9)],
                                                    rm_indel_inducing_pattern = F,
                                                    type = "local",
                                                    seq_type = "NT",
                                                    algnmt_plot_args = list(add_length_suffix = T,
                                                                            plot.pattern.names = T,
                                                                            plot.pattern.names_fun = ggplot2::geom_text,
                                                                            group_on_yaxis = T,
                                                                            min_gap = 1000))
alignment[["match.plot"]]

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
                                                    patterns = c(test_pattern, test_pattern, test_pattern2),
                                                    rm_indel_inducing_pattern = F,
                                                    type = "local")


alignment[["match.plot"]] + ggplot2::xlim(c(alignment[["min.max.subject.position"]][1] - 100,
                                            alignment[["min.max.subject.position"]][2] + 500))

algn <- Biostrings::pairwiseAlignment(subject = Biostrings::DNAStringSet(ncbi_data$origin),
                                      pattern = Biostrings::DNAStringSet(seqs[2]),
                                      type = "global-local")

# this is slow for large algnt
igsc::printPairwiseAlignment(algn, "/Users/vonskopnik/Documents/R_packages/brathering/inst/extdata/test.txt")



