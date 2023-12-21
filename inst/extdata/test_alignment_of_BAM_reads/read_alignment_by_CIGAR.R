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
reads_sub <- reads %>% dplyr::filter(cigar == "98M") %>% dplyr::slice_sample(n = 20)
reads_sub <- reads %>% dplyr::filter(cigar != "98M") %>% dplyr::slice_sample(n = 20)

# check at which position the reads match
## reads on - strand need reverse complement; if revcomp_minus_strand in reads_from_bam is TRUE, if FALSE
#check_al <- Biostrings::matchPattern(pattern = reads_sub$seq[1], subject = chr11, max.mismatch = 10) # on + strand
#check_al@ranges
#check_al <- Biostrings::matchPattern(pattern = as.character(Biostrings::reverseComplement(Biostrings::DNAString(reads_sub$seq[2]))), subject = chr11, max.mismatch = 10) # on - strand; but only if revcomp_minus_strand = TRUE
#check_al@ranges


pattern_df <- purrr::pmap_dfr(list(reads_sub$start, reads_sub$seq, reads_sub$readName), function(x,y,z) data.frame(seq = strsplit(y, "")[[1]], position = x:(x+97), seq.name = z))
algnmt_df <- data.frame(seq = strsplit(chr11_cd81, "")[[1]], position = 2378344:2391242, seq.name = "chr11")
algnmt_df <- rbind(algnmt_df, pattern_df)
algnmt_df_wide <- tidyr::pivot_wider(algnmt_df, names_from = seq.name, values_from = seq) %>% tidyr::drop_na(3)

plot <- igsc::algnmt_plot(algnmt = algnmt_df,
                          algnmt_type = "NT",
                          ref = "chr11")
ggsave(plot, filename = "al_plot.pdf", device = cairo_pdf, path = wd, height = 3, width = 12)


pattern_df <- purrr::pmap_dfr(list(reads_sub$cigar, reads_sub$start, reads_sub$seq, reads_sub$readName), function(x,y,z,a) cigar_to_position(x,y,z,a))
algnmt_df <- data.frame(seq = strsplit(chr11_cd81, "")[[1]], position = 2378344:2391242, seq.name = "chr11")
algnmt_df <- rbind(algnmt_df, pattern_df)
plot <- igsc::algnmt_plot(algnmt = algnmt_df,
                          algnmt_type = "NT",
                          ref = "chr11",
                          line = T)
ggsave(plot, filename = "al_plot.pdf", device = cairo_pdf, path = wd, height = 3, width = 12)


## the principle works now, position as returned by STAR aligner starts at 1 for every chromosome
## now try converting the CIGAR strings

cigar <- "16S67M5729N15M"
start <- 2384616
seq <- "TTTTTCTTATATGGGGAGCGGGCGCCTCCGGAGGCTGGAGTATCTTGGGGGGGGGGAGCAGGTGGCAGAGAGGCTTCCCACAGCTGGCTGGAGGCGTG"
cigar_to_position <- function(cigar, start, seq, name = NULL, name_col = "seq.name") {
    # https://davetang.org/wiki/tiki-index.php?page=SAM
    # https://github.com/NBISweden/GAAS/blob/master/annotation/knowledge/cigar.md

    # clipping means that respective bases were not uses for the alignment, but are retained in the output
    # usually at the end of reads
    # maybe because sequencing errors are more likely towards the ends?!
    cigar_split <- strsplit(cigar, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", perl=TRUE)[[1]]
    val <- c(0,as.integer(cigar_split[grep("\\d", cigar_split, perl=TRUE)]))
    op <- cigar_split[grep("\\D", cigar_split, perl=TRUE)]
    val_cum <- cumsum(val)

    # these are the positions in seq
    val_seq <- val[-1][which(op %in% c("S", "M", "=", "X"))]
    val_seq_cum <- c(0,cumsum(val_seq))

    seq_df <- data.frame(seq = character(val_cum[length(val_cum)]), position = seq(start, start+val_cum[length(val_cum)]-1))
    # i is counter for op
    # j is counter for val_seq_cum
    j <- 1
    for (i in seq_along(op)) {
        if (op[i] %in% c("S", "M")) {
            seq_df$seq[(val_cum[i]+1):val_cum[i+1]] <- strsplit(substr(seq, val_seq_cum[j]+1, val_seq_cum[j+1]), "")[[1]]
            j <- j + 1
        }
        if (op[i] == "N") {
            seq_df$seq[(val_cum[i]+1):val_cum[i+1]] <- rep(NA, val[i+1])
            # j remains the same
        }
        if (op[i] %in% c("I", "D", "H")) {
            message("New operation found in cigar string. index: ", i)
            stop("New operation found in cigar string.")
        }

        #if I, D, H missing

    }

    if (!is.null(name)) {
        seq_df[,name_col] <- name
    }
    return(seq_df)
}

# from chat gpt:
# Function to parse CIGAR string and assign positions to nucleotides
assign_positions <- function(sequence, cigar_string) {
    # Initialize variables
    positions <- numeric(0)
    current_position <- 1

    # Split CIGAR string into operations and lengths
    cigar_tokens <- gregexpr("[0-9]+[A-Z=]", cigar_string, perl=TRUE)[[1]]
    operations <- regmatches(cigar_string, cigar_tokens)
    lengths <- as.integer(gsub("[A-Z=]", "", operations))

    # Loop through CIGAR operations
    for (i in seq_along(operations)) {
        operation <- substr(operations[i], nchar(operations[i]), nchar(operations[i]))
        length <- lengths[i]

        # Update positions based on CIGAR operation
        if (operation %in% c("M", "=")) {
            positions <- c(positions, current_position:(current_position + length - 1))
            current_position <- current_position + length
        } else if (operation == "D") {
            positions <- c(positions, rep(NA, length))
            current_position <- current_position + length
        } else if (operation == "I") {
            current_position <- current_position + length
        } else if (operation == "S") {
            # Soft clipping, positions are not affected
        } else {
            stop("Unsupported CIGAR operation: ", operation)
        }
    }

    # Create a data frame with positions and nucleotides
    result <- data.frame(position = positions, nucleotide = strsplit(sequence, "")[[1]])
    return(result)
}

# Example usage
sequence <- "ATCGTACG"
cigar_string <- "2=1X3=1D2I"
result <- assign_positions(sequence, cigar_string)
print(result)

