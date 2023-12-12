## viral reads mapped
library(magrittr)
library(ggplot2)
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)

HHV_ref_genome <- igsc::read_fasta("/Volumes/CMS_SSD_2TB/HHV_ref/genome.fa")
HHV_gtf <- vroom::vroom_lines("/Volumes/CMS_SSD_2TB/HHV_ref/genes.gtf", skip = 5)

bam_path <- "/Volumes/4TB_Backup_Geraet/EBV_ctrl_HHV_virus_ref_only/outs/possorted_genome_bam.bam"

nchar(HHV_ref_genome)

NC_007605_range <- GenomicRanges::GRanges(seqnames = "NC_007605.1", strand = "+", ranges = IRanges::IRanges(start = c(1), end = c(171823)))

NC_007605_read_df <- scexpr::reads_from_bam(file_path = bam_path, genomic_ranges = NC_007605_range)


algnmnt <- MultiplePairwiseAlignmentsToOneSubject(subject = HHV_ref_genome[["NC_007605.1"]],
                                                  patterns = NC_007605_read_df[1,"seq"],
                                                  type = "global-local",
                                                  seq_type = "NT")

algnmnt[["plot"]][["match_match"]] + xlim(algnmnt[["min.max.subject.position"]][1]-20, algnmnt[["min.max.subject.position"]][2]+20)


algnmnt <- MultiplePairwiseAlignmentsToOneSubject(subject = HHV_ref_genome[["NC_007605.1"]],
                                                  patterns = NC_007605_read_df[1:10,"seq"],
                                                  type = "global-local",
                                                  seq_type = "NT")

algnmnt[["plot"]][["match_match"]] + xlim(algnmnt[["min.max.subject.position"]][1]-20, algnmnt[["min.max.subject.position"]][2]+20)


GenomicAlignments::cigar(NC_007605_read_df[1,"seq"])



# Function to parse CIGAR string and obtain alignment coordinates
parseCIGAR <- function(cigar_string, start_position) {
    cigar <- strsplit(cigar_string, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", perl=TRUE)[[1]]
    cigar_values <- as.integer(cigar[grep("\\d", cigar, perl=TRUE)])
    cigar_operations <- cigar[grep("\\D", cigar, perl=TRUE)]

    ref_pos <- start_position
    query_pos <- 1

    for (i in seq_along(cigar_values)) {
        op <- cigar_operations[i]
        len <- cigar_values[i]

        if (op %in% c("M", "D", "N")) {
            ref_pos <- ref_pos + len
        }

        if (op %in% c("M", "I", "S")) {
            query_pos <- query_pos + len
        }
    }

    return(list(reference = ref_pos - 1, query = query_pos - 1))
}

# Example usage
cigar_string <- "3M1I2D4M"
start_position <- 100
alignment_coordinates <- parseCIGAR(cigar_string, start_position)

parseCIGAR(cigar_string = NC_007605_read_df[1,"cigar"], NC_007605_read_df[1,"start"])

my_string <- "abc123def456ghi789"

# Split the string at any numeric character
# even: class
# uneven: length
unlist(strsplit(my_string, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", perl = TRUE))


