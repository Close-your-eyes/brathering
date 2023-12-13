library(rentrez)

# prep the genome character (make separate txt file manually)
genome <- vroom::vroom_lines("/Volumes/CMS_SSD_2TB/EBV_genome.txt")
genome <- genome[-1]
genome <- paste(genome, collapse = "")
nchar(genome)

# EBV: NC_007605.1
# CMV: NC_006273.2
# HSV1: NC_001806.2
# HSV2: NC_001798.2
# VZV: NC_001348.1

# get data from ncbi, whole page as one character
gene_data_ncbi <- rentrez::entrez_fetch("nucleotide", "NC_007605.1", rettype = "text")
gene_data_ncbi <- gsub("\n", "", gene_data_ncbi)
gene_data_ncbi <- gsub(" {1,}", "", gene_data_ncbi)

# split for CDS and mRNA and record all results
df <- purrr::map_dfr(setNames(c("mRNA", "CDS"), c("mRNA", "CDS")), function(x) {
  prep_sequence_df(strsplit(gene_data_ncbi, x)[[1]], genome = genome)
}, .id = "type")

# prepare lines for writing to genome.fa
lines_to_write <- sapply(1:nrow(df), function(x) c(paste0(">", df[x,"locus_tag"]), splitString(df[1,"seq"], n = 60)), simplify = F)
lines_to_write <- unlist(lines_to_write)

## then add to genome from 10X
vroom::vroom_write_lines(lines_to_write, "/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A/fasta/genome.fa", append = T)
# check
# genomes_vect <- vroom::vroom_lines("/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A/fasta/genome.fa") # working!
#tail(genomes_vect)
#tail(lines_to_write)

## prep for GTF file
lines_to_write_gft <- sapply(1:nrow(df), function(x) {
  paste(c(df[x,"locus_tag"], "unknown", "exon", "1", as.character(nchar(df[x,"seq"])), ".", "+", ".", paste0("gene_id \"", df[x,"locus_tag"], "\"; ", "transcript_id \"", df[x,"locus_tag"], "\";")), collapse = "\t")
})
vroom::vroom_write_lines(lines_to_write_gft, "/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A/genes/genes.gtf", append = T)

tail(GTF_10X)
#GTF_10X <- vroom::vroom_lines("/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A/genes/genes.gtf")
#head(GTF_10X)
#strsplit(GTF_10X[6], "\\\t")
#exp <- 'GFP\tunknown\texon\t1\t922\t.\t+\t.\tgene_id "GFP"; transcript_id "GFP"; gene_name "GFP"; gene_biotype "protein_coding";'
#strsplit(exp, "\\\t")

'A semicolon-delimited list of key-value pairs of the form key "value".
The attribute keys transcript_id and gene_id are required; gene_name is optional and may be non-unique,
but if present will be preferentially displayed in reports.'


prep_sequence_df <- function(y, genome) {
  out <- lapply(y, function(x) {
    if (grepl("gene", x) && grepl("locus_tag", x) && grepl("GeneID", x) && (grepl("^complement", x) | grepl("^join", x) | grepl("^[[:digit:]]", x))) {
      # retrieve all segments to join
      segments <- get_segments(x)
      # concat the sequence from segments
      sequence <- paste(lapply(segments[[1]], function(x) substr(genome, x[1], x[2])), collapse = "")
      if (segments[[2]]) {
        sequence <- as.character(Biostrings::reverseComplement(Biostrings::DNAString(sequence)))
      }
      
      return(data.frame(seq = sequence,
                        name = get_name(x),
                        locus_tag = get_locus_tag(x)))
    }
  })
  out <- out[!sapply(out, is.null)]
  out <- dplyr::bind_rows(out)
  return(out)
}

get_segments <- function(x) {
  segments <- strsplit(x, "/gene=")[[1]][1]
  complement <- F
  if (grepl("join", segments)) {
    if (grepl("complement", segments)) {
      segments <- strsplit(strsplit(gsub("\\)", "", gsub("complement\\(join\\(", "", segments)), ",")[[1]], "\\.\\.")
      complement <- T
    } else {
      segments <- strsplit(strsplit(gsub("\\)", "", gsub("join\\(", "", segments)), ",")[[1]], "\\.\\.")
    }
  } else {
    if (grepl("complement", segments)) {
      segments <- strsplit(strsplit(gsub("\\)", "", gsub("complement\\(", "", segments)), ",")[[1]], "\\.\\.")
      complement <- T
    } else {
      segments <- strsplit(segments, "\\.\\.")
    }
  }
  return(list(segments = segments, complement = complement))
}

get_name <- function(x) {
  name <- strsplit(strsplit(x, "gene=")[[1]][2], "locus_tag")[[1]][1]
  name <- substr(name, 2, nchar(name)-2)
  name <- make.names(name)
  name <- gsub("\\.", "", name)
  return(name)
}

get_locus_tag <- function(x) {
  locus_tag <- strsplit(strsplit(strsplit(x, "locus_tag=")[[1]][2], "db_xref")[[1]][1], "note=")[[1]][1]
  locus_tag <- substr(locus_tag, 2, nchar(locus_tag)-2)
  locus_tag <- strsplit(locus_tag, "\"/")[[1]][1]
  locus_tag <- make.names(locus_tag)
  locus_tag <- gsub("\\.", "", locus_tag)
  return(locus_tag)
}

splitString <- function(inputString, n) {
  # Split the input string into individual characters
  chars <- strsplit(inputString, '')[[1]]
  
  # Calculate the number of substrings
  numSubstrings <- ceiling(length(chars) / n)
  
  # Create a matrix to store substrings
  substrings <- matrix('', nrow = numSubstrings, ncol = n)
  
  # Populate the matrix with substrings
  for (i in seq_along(chars)) {
    substrings[(i - 1) %/% n + 1, i %% n + 1] <- chars[i]
  }
  
  # Convert matrix to a list of strings
  result <- apply(substrings, 1, paste, collapse = '')
  
  # Remove empty strings
  result <- result[result != '']
  
  return(result)
}



# this works, but do it with vroom
#line="blah text blah blah etc etc"
#write(line,file="/Volumes/CMS_SSD_2TB/refdata-gex-GRCh38-2020-A/fasta/genome.fa",append=TRUE)

