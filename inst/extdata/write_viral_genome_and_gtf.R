wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
library(brathering)
library(magrittr)

viral_genome_accessions <- c("NC_007605.1",
                             "NC_006273.2",
                             "NC_001806.2",
                             "NC_001798.2",
                             "NC_001348.1")

ncbi_data_list0 <- lapply(setNames(viral_genome_accessions, viral_genome_accessions), function(x) {
    igsc::webscrape_ncbi(accession = x)
})

ncbi_data_list <- mapply(x = ncbi_data_list0, y = c("HHV4", "HHV5", "HHV1", "HHV2", "HHV3"),  function(x,y) {
    x[["values_to_genome_gtf"]] <- unique(x[["features"]] %>% dplyr::filter(grepl(y, value)) %>% dplyr::pull(value))
    x[["features_to_genome_gtf"]] <- setNames(c("exon"), c("CDS"))
    return(x)
}, SIMPLIFY = F)


## function has changed: check!
ncbi_data_list <- purrr::map(ncbi_data_list, function(x) {
    x[["features"]] <-
        x[["features"]] %>%
        dplyr::filter(Subfeature == "locus_tag")
    return(x)
})
'write_gtf_and_genome(data = ncbi_data_list,
                     genome_file_path = file.path("/Volumes/CMS_SSD_2TB/HHV_ref", "genome.fa"),
                     gtf_file_path = file.path("/Volumes/CMS_SSD_2TB/HHV_ref", "genes.gtf"))

write_gtf_and_genome(ncbi_data_list = ncbi_data_list,
                     genome_file_path = file.path("/Users/vonskopnik/Documents/R_packages/brathering/inst/extdata/HHV_ref", "genome.fa"),
                     gtf_file_path = file.path("/Users/vonskopnik/Documents/R_packages/brathering/inst/extdata/HHV_ref", "genes.gtf"))
'


write_gtf_and_genome_for_cellranger(data = ncbi_data_list,
                                    save_path = file.path(wd, "HHV_ref3"))


## other entry via local genome data - check the illumina assay analysis procedure.
## write documentation on the data argument for write_gtf_and_genome_for_cellranger


