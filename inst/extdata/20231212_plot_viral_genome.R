wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
library(brathering)
library(magrittr)
ncbi_data <- webscrape_ncbi(accession = "NC_007605.1")
feat <- ncbi_data[["features"]]


## prepare data frame for algnmt_plot from feat_table
## convert CIGAR String to ncbi annotation


feat_sub <-
    feat %>%
    dplyr::filter(Feature == "CDS") %>%
    dplyr::filter(Subfeature == "locus_tag")


data <- get_seqs_from_feature_df(feature_df = feat_sub[1:10,],
                                 origin = ncbi_data$origin,
                                 order_features = T)


tt <- MultiplePairwiseAlignmentsToOneSubject(subject = ncbi_data$origin,
                                             patterns = data[["sequences"]][1:5],
                                             algnmt_plot_args = list(add_length_suffix = T, pattern_lim_size = 2, group_on_yaxis = T))

# TODO: add option to plot lines from start to end of one algmt (e.g. to visualize small pieces that belong together)
al_plot <- algnmt_plot(algnmt = data[["df_long"]][[1]],
                       algnmt_type = "NT",
                       line = T,
                       pattern_lim_size = 2,
                       pattern_names = T,
                       add_length_suffix = T,
                       subject_lim_lines = T,
                       pos_shift = "+40000",
                       group_on_yaxis = T)


al_plot + ggplot2::xlim(25000,80000)
