wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
library(brathering)
library(magrittr)
ncbi_data <- webscrape_ncbi(accession = "NG_011608.2")
feat <- ncbi_data[["features"]]

mRNA <- dplyr::filter(feat, Feature %in% c("CDS", "mRNA"), Subfeature == "gene")
mRNA_seq <- get_seqs_from_feature_df(feature_df = mRNA, origin = ncbi_data$origin, concat = F,
                                     min_to_max_boundary = T)


# gtf files contains separate rows for gene, transcript, exon, etc.
#
gtf <- vroom::vroom("/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A_viral_mod/genes.gtf", skip = 5, n_max = 500, col_names = F)
gtf2 <- vroom::vroom("/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A_viral_mod/genes.gtf", n_max = 20, col_names = F)
sub <- gtf[1:10,]
strsplit(gtf[1,9,drop=T], ";")

feat_sub <- dplyr::filter(feat, Subfeature == "gene")


# how to handle genes from a virus genome? exons have to be added to gtf in order to make CellRanger regcognize them
ncbi_data2 <- webscrape_ncbi(accession = "NC_001348.1")
feat2 <- ncbi_data2[["features"]]

ncbi_data_list <- list(ncbi_data, ncbi_data2)


exons <- dplyr::filter(feat, Feature == "exon", Subfeature == "number")
exons_seqs <- get_seqs_from_feature_df(feature_df = exons, origin = ncbi_data$origin, concat = F)
misc_features <- dplyr::filter(feat, Feature == "misc_feature", Subfeature == "note")
misc_features_seq <- get_seqs_from_feature_df(feature_df = misc_features, origin = ncbi_data$origin, concat = F)
sig_peptides <- dplyr::filter(feat, Feature == "sig_peptide", Subfeature == "note")
sig_peptides_seq<- get_seqs_from_feature_df(feature_df = sig_peptides, origin = ncbi_data$origin, concat = F)

CDSs <- dplyr::filter(feat, Feature == "CDS", Subfeature == "gene")
CDSs_seq<- get_seqs_from_feature_df(feature_df = CDSs, origin = ncbi_data$origin, concat = F)

# intron at ncbi: what does 'order' mean?

pattern_list <- list(exons = unlist(exons_seqs),
                     sig_peptide = unlist(sig_peptides_seq),
                     misc = unlist(misc_features_seq),
                     CDS = unlist(CDSs_seq))

alignment <- MultiplePairwiseAlignmentsToOneSubject(subject = substr(ncbi_data$origin, 20000, nchar(ncbi_data$origin)),
                                                    patterns = pattern_list,
                                                    rm_indel_inducing_pattern = F,
                                                    type = "local",
                                                    algnmt_plot_args = list(add_length_suffix = T,
                                                                            plot.pattern.names = T,
                                                                            plot.pattern.names_fun = ggplot2::geom_text))
alignment[["match.plot"]]
