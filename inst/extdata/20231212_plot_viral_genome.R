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


data <- get_seqs_from_feature_df(feature_df = feat_sub,
                                 origin = ncbi_data$origin)
data_long <-
    data %>%
    tidyr::pivot_longer(cols = -position, names_to = "seq.name", values_to = "seq")

algnmt_plot <- igsc::algnmt_plot(algnmt = data[["df_long"]][[1]], algnmt_type = "NT", ) # add_length_suffix = T causes error ?!
