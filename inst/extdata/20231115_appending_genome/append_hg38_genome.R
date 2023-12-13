wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
source(file.path(wd, "append_hg38_genome_funs.R"))

genome_file <- "/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A_viral_mod/genome.fa"
gtf_file <- "/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A_viral_mod/genes.gtf"

# EBV: NC_007605.1
# CMV: NC_006273.2
# HSV1: NC_001806.2
# HSV2: NC_001798.2
# VZV: NC_001348.1

viral_genome_accessions <- c("NC_007605.1",
                             "NC_006273.2",
                             "NC_001806.2",
                             "NC_001798.2",
                             "NC_001348.1")

dfs <- lapply(setNames(viral_genome_accessions, viral_genome_accessions), function(zzz) {

  gene_data_ncbi <- gsub(" {1,}", "", gsub("\n", "", rentrez::entrez_fetch("nucleotide", zzz, rettype = "text")))
  genome <- paste(vroom::vroom_lines(grep(zzz, list.files("/Volumes/CMS_SSD_2TB/20231115_appending_genome/viral_genomes", full.names = T), value = T))[-1], collapse = "")

  test <- prep_sequence_df(strsplit(gene_data_ncbi, x)[[1]], genome = genome)

  # split for CDS and mRNA and record all results
  df <- purrr::map_dfr(setNames(c("mRNA", "CDS"), c("mRNA", "CDS")), function(x) {
    prep_sequence_df(strsplit(gene_data_ncbi, x)[[1]], genome = genome)
  }, .id = "type")

  # error check for dups in fun!
  df <- dplyr::distinct(df, locus_tag, type, .keep_all = T)

  #append_genome(df, genome_file)
  #append_gtf(df, gtf_file)

  return(df)
})

# make function to plot gene organisation of virusses
# viral genome as template, then gene segments and their orientation as arrows (gggenomes?)



# manually add CD81 and CD84 mRNA and CDS as positive control (sample NR_JK_053)
df <- data.frame(type = c("mRNA", "CDS", "mRNA", "CDS"),
                 seq = c(igsc::read_fasta("/Volumes/CMS_SSD_2TB/20231115_appending_genome/viral_genomes/NM_004356.4_CD81_mRNA.txt"),
                         igsc::read_fasta("/Volumes/CMS_SSD_2TB/20231115_appending_genome/viral_genomes/NM_004356.4_CD81_CDS.txt"),
                         igsc::read_fasta("/Volumes/CMS_SSD_2TB/20231115_appending_genome/viral_genomes/NM_001184879.2_CD84_mRNA.txt"),
                         igsc::read_fasta("/Volumes/CMS_SSD_2TB/20231115_appending_genome/viral_genomes/NM_001184879.2_CD84_CDS.txt")),
                 name = c("CD81", "CD81", "CD84", "CD84"),
                 locus_tag = c("CD81", "CD81", "CD84", "CD84"))
append_genome(df, genome_file)
append_gtf(df, gtf_file)





## testing
# CD81
zzz <- "NM_004356.4"
gene_data_ncbi <- gsub(" {1,}", "", gsub("\n", "", rentrez::entrez_fetch("nucleotide", zzz, rettype = "text")))
genome <- paste(vroom::vroom_lines(grep(zzz, list.files("/Volumes/CMS_SSD_2TB/20231115_appending_genome/viral_genomes", full.names = T), value = T)[2])[-1], collapse = "") # index 2 = mRNA file

## extract sequence from gene_data_ncbi
# this can now replace reading genomes from extra text files
# one could also save this to fasta files
seq <- strsplit(gene_data_ncbi, "ORIGIN")[[1]]
seq <- seq[length(seq)] # get last index in any case
seq <- toupper(gsub("//", "", gsub("[[:digit:]]{1,}", "", seq)))

# split for CDS and mRNA and record all results
df <- purrr::map_dfr(setNames(c("mRNA", "CDS"), c("mRNA", "CDS")), function(x) {
  prep_sequence_df(strsplit(gene_data_ncbi, x)[[1]], genome = genome)
}, .id = "type")

test <- strsplit(gene_data_ncbi, x)[[1]]

test <- vroom::vroom_lines("/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A_viral_mod/genome.fa")
tail(test)
test[(length(test)-1000):(length(test))]


test <- vroom::vroom_lines("/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A_viral_mod/genes.gtf")
tail(test)
test[(length(test)-100):(length(test))]







