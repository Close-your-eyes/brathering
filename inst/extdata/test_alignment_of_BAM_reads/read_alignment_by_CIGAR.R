## test to align reads from BAM file by CIGAR string conversion to reference
library(magrittr)
library(ggplot2)
#install.packages("fst")
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
# see: /Users/vonskopnik/Documents/R_packages/brathering/inst/extdata/20231115_appending_genome/working_with_SAM_BAM_files_in_R.R
bamfile_path <- "/Volumes/CMS_SSD_2TB/R_scRNAseq/2019_SLE_LN/data/Sequencing_data/hg38_CR7_wo_introns/GEX/Pat7_rep1_urine/outs/possorted_genome_bam.bam"

# get ref
#genome <- vroom::vroom_lines("/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A_viral_mod/genome.fa", n_max = 9000000)
#chr_names <- genome[which(grepl("^>", genome))]
#start_line_chr11 <- which(genome == ">chr11 11")
#end_line_chr11 <- which(genome == ">chr12 12")


start_line_chr11 <- 6379235
end_line_chr11 <- 8630680
chr11 <- vroom::vroom_lines("/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A_viral_mod/genome.fa", skip = start_line_chr11,
                            n_max = end_line_chr11-start_line_chr11+1)
# final line is chr12: chr11[which(grepl("^>", chr11))]
chr11 <- chr11[-length(chr11)]
chr11 <- paste(chr11, collapse = "")
chr11_cd81 <- substr(chr11, 2378344, 2391242)

# chr11:2,378,344-2,391,242
# reads from this range exported from IGV:
# here we have CIGAR strings, could check with them as well
igv_cd81_export <- vroom::vroom(file.path(wd, "CD81_test_visibleData.sam"), skip = 203, col_names = F, delim = "\t")

# compare with what is return by RSamtools
# do position start a 1 or 0 for every chromosome?
#2378344 - 2391242
cd81_range <- GenomicRanges::GRanges(seqnames = "chr11", strand = "+", ranges = IRanges::IRanges(start = c(2378344), end = c(2391242)))

# check scexpr fun? CIGAR not returned?!
reads <- scexpr::reads_from_bam(bamfile_path, genomic_ranges = cd81_range, revcomp_minus_strand = F)

#reads_sub <- reads %>% dplyr::filter(cigar == "98M") %>% dplyr::filter(grepl("I", cigar))
#reads_sub <- reads %>% dplyr::filter(cigar == "98M") %>% dplyr::slice_sample(n = 20)
#reads_sub <- reads %>% dplyr::filter(cigar != "98M") %>% dplyr::slice_sample(n = 20)
reads_sub <- reads %>% dplyr::slice_sample(n = 40)


pattern_df <- purrr::pmap_dfr(list(reads_sub$cigar, reads_sub$start, reads_sub$seq, reads_sub$readName), function(x,y,z,a) igsc::cigar_to_position(x,y,z,a, rm_clipped = T))
algnmt_df <- data.frame(seq = strsplit(chr11_cd81, "")[[1]], position = 2378344:2391242, seq.name = "chr11")
algnmt_df <- rbind(algnmt_df, pattern_df)
plot <- algnmt_plot(algnmt = algnmt_df,
                    algnmt_type = "NT",
                    ref = "chr11",
                    group_on_yaxis = F, # error when TRUE
                    line = T)
ggsave(plot, filename = "al_plot.pdf", device = cairo_pdf, path = wd, height = 6, width = 12)
algnmt_df_compares <- igsc::compare_seq_df_long(algnmt_df %>% dplyr::filter(seq.name == "chr11" | grepl("19542", seq.name)), change_pattern = T, ref = "chr11", seq_original = NULL,
                                                pattern_mismatch_as = "base") %>%
    tidyr::pivot_wider(names_from = seq.name, values_from = seq) %>%
    tidyr::drop_na()

