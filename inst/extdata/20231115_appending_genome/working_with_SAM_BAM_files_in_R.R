# test
library(magrittr)

bamfile_path <- "/Volumes/CMS_SSD_2TB/R_scRNAseq/2019_SLE_LN/data/Sequencing_data/hg38_CR7_wo_introns/GEX/Pat7_rep1_urine/outs/possorted_genome_bam.bam"


gtf <- vroom::vroom("/Volumes/4TB_Backup_Geraet/Reference_genomes/refdata-gex-GRCh38-2020-A/genes/genes.gtf", skip = 5, col_names = F)

nrow(gtf)/100000

cd81 <- which(grepl("CD81", gtf$X9))
gtf_d81 <- gtf[cd81,]
# start and end positions roughly the positions in IGV browser





# chr11:2,378,344-2,391,242
# reads from this range exported from IGV:
igv_cd81_export <- vroom::vroom("/Volumes/CMS_SSD_2TB/CD81_test_visibleData.sam", skip = 203, col_names = F, delim = "\t")

# compare with what is return by RSamtools
2378344 - 2391242
cd81_range <- GenomicRanges::GRanges(seqnames = "chr11", strand = "+", ranges = IRanges::IRanges(start = c(2378344), end = c(2391242)))
reads <- scexpr::reads_from_bam(bamfile_path, genomic_ranges = cd81_range)

## strand definition is irrelevant in GRanges
#cd81_range_minus <- GenomicRanges::GRanges(seqnames = "chr11", strand = "-", ranges = IRanges::IRanges(start = c(2378344), end = c(2391242)))
#reads_minus <- scexpr::reads_from_bam(bamfile_path, genomic_ranges = cd81_range_minus)
#reads_minus$readName %in% reads$readName # all the same

## all reads from IGV export are also returned by Rsamtools
## but Rsamtools return many more
igv_cd81_export$X1 %in% reads$readName

reads_contained <-
  reads %>%
  dplyr::filter(readName %in% igv_cd81_export$X1)

reads_not_contained <-
  reads %>%
  dplyr::filter(!readName %in% igv_cd81_export$X1)

sort(setNames(c(min(reads_contained$start), max(reads_contained$start), min(reads_not_contained$start), max(reads_not_contained$start)), nm = c("contained_min", "contained_max",
                                                                                                                                           "not_contained_min", "not_contained_max")))


# compare with what is return by RSamtools

## how to work with sequences?
## different alignment strategy?
## pull data from ncbi quickly and plot position of exons with igsc function, then add aligned reads on top
## igsc function: return which reads (indices) exactly cause indels
## when gene is on minus or plus strand - how to convert reads? which reads to convert with reverse complement?
## why do the the exonic reads returned by igv do not match well, but those only return by Rsamtools do?

cd81_gene_ncbi <- igsc::read_fasta("/Volumes/CMS_SSD_2TB/20231115_appending_genome/cd81_gene.fasta")
cd81_mRNA <- igsc::read_fasta("/Volumes/CMS_SSD_2TB/20231115_appending_genome/viral_genomes/NM_004356.4_CD81_mRNA.txt")
reads_contained_exonic <- dplyr::filter(reads_contained, RE == "E")
out <- igsc::MultiplePairwiseAlignmentsToOneSubject(subject = cd81_mRNA, patterns = Biostrings::reverseComplement(Biostrings::DNAStringSet(reads_contained_exonic$seq[15:25])))
out[["match.plot"]]
out <- igsc::MultiplePairwiseAlignmentsToOneSubject(subject = cd81_mRNA, patterns = reads_contained_exonic$seq[1:10], type = "local")
out[["match.plot"]]

reads_not_contained_exonic <-  dplyr::filter(reads_not_contained, RE == "E")
out <- igsc::MultiplePairwiseAlignmentsToOneSubject(subject = cd81_mRNA, patterns = Biostrings::reverseComplement(Biostrings::DNAStringSet(reads_not_contained_exonic$seq[6:10]))) # this works!
out[["match.plot"]]
out <- igsc::MultiplePairwiseAlignmentsToOneSubject(subject = cd81_mRNA, patterns = reads_not_contained_exonic$seq[1:10])
out[["match.plot"]]


