

## make this more general to also allow parsing mRNA or Gene information
## then pull our CDS and exons etc.
## more general: change genomes to "sequence"

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


append_genome <- function(df, genome_file_path) {
  ## checks checks checks needed
  
  # prepare lines for writing to genome.fa
  lines_to_write <- sapply(1:nrow(df), function(x) c(paste0(">", df[x,"locus_tag",drop=T], "_", df[x,"type",drop=T]),
                                                     splitString(df[x,"seq",drop=T], n = 60)), simplify = F)
  ## then add to genome from 10X
  vroom::vroom_write_lines(unlist(lines_to_write), genome_file_path, append = T)
}

append_gtf <- function(df, gtf_file_path) {
  
  lines_to_write_gft <- sapply(1:nrow(df), function(x) {
    paste(c(paste0(df[x,"locus_tag",drop=T], "_", df[x,"type",drop=T]), "unknown", "exon", "1", as.character(nchar(df[x,"seq",drop=T])), ".", "+", ".", paste0("gene_id \"", df[x,"locus_tag",drop=T], "_", df[x,"type",drop=T], "\"; ", "transcript_id \"", df[x,"locus_tag",drop=T], "_", df[x,"type",drop=T], "\";")), collapse = "\t")
  })
  vroom::vroom_write_lines(lines_to_write_gft, gtf_file_path, append = T)
}



