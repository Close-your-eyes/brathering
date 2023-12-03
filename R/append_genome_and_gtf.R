append_genome_and_gtf <- function(feature_df, origin, genome_file_path, gtf_file_path) {

  ## checks checks checks needed

  seqs <- get_seqs_from_feature_df(feature_df = feature_df, origin = origin, concat = T, make_revcomp = T)

  # prepare lines for writing to genome
  # then write with vroom
  lines_to_write <- sapply(seq_along(seqs), function(x) c(paste0(">", feat_sub$value[x], "_", feat_sub$Feature[x]), splitString(seqs[[x]], n = 60)), simplify = F)
  vroom::vroom_write_lines(unlist(lines_to_write), genome_file_path, append = T)

  lines_to_write_gft <- sapply(seq_along(seqs), function(x) paste(c(paste0(feat_sub$value[x], "_", feat_sub$Feature[x]), "unknown", "exon", "1", as.character(nchar(seqs[[x]])), ".", "+", ".", paste0("gene_id \"", feat_sub$value[x], "_", feat_sub$Feature[x], "\"; ", "transcript_id \"", feat_sub$value[x], "_", feat_sub$Feature[x], "\";")), collapse = "\t"))
  vroom::vroom_write_lines(lines_to_write_gft, gtf_file_path, append = T)


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
