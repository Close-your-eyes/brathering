write_gtf_and_genome <- function(ncbi_data_list,
                                 genome_file_path,
                                 gtf_file_path,
                                 gtf_header = c("##description: own ref", "##provider: CMS", "##conctact: vonskopnik@pm.me", "##format: gtf", paste0("##date: ", Sys.Date())),
                                 append = F,
                                 overwrite = F) {

    # write_gtf_and_genome fun has to write a header for gtf
    # in order to provide the origin, ncbi_data has to be passed to that fun
    #EXTRA:
    # cut origin to gene or transcript region before addition to gtf file? - then start of ranges would for exons or mRNA needed adjustment

    # allow to pass a list of ncbi_data objects to write_gtf_and_genome
    # mRNA becomes transcript
    # complement == TRUE --> minus strand in X9?
    # from range column, split by , and use smallest and largest value as boundary to add to gtf
    # add a definition to each ncbi_data list entry which entries to add as exons and which to as as transcript

    # example:
    'ncbi_data <- webscrape_ncbi(accession = "NG_011608.2")
    ncbi_data[["features_to_genome_gtf"]] <- setNames(c("exon", "transcript"), c("exon", "mRNA")) # conversion from to
    ncbi_data[["values_to_genome_gtf"]] <- "CD8A"
    # ncbi_data[["origin_name"]] <- "CD8A" # optional
    ncbi_data2 <- webscrape_ncbi(accession = "NC_001348.1") # HHV3
    ncbi_data2[["features_to_genome_gtf"]] <- setNames(c("exon"), c("CDS"))
    ncbi_data2[["values_to_genome_gtf"]] <- c("HHV3_gp01", "HHV3_gp02", "HHV3_gp03")
    ncbi_data_list <- list(ncbi_data, ncbi_data2)'

    if (file.exists(gtf_file_path) && append && !overwrite) {
        stop("gtf file exists and must be appended. Set append to TRUE or select another file. Or set overwrite = TRUE.")
    }
    if (file.exists(genome_file_path) && append && !overwrite) {
        stop("genome file exists and must be appended. Set append to TRUE or select another file. Or set overwrite = TRUE.")
    }

    if (basename(genome_file_path) != "genome.fa") {
        stop("genome file has to be named 'genome.fa")
    }
    if (basename(gtf_file_path) != "genes.gtf") {
        stop("gtf file has to be named 'genes.gtf")
    }

    dir.create(dirname(genome_file_path), showWarnings = F, recursive = T)
    dir.create(dirname(gtf_file_path), showWarnings = F, recursive = T)


    if (any(y <- !sapply(lapply(ncbi_data_list, names), function(x) "features" %in% x && "origin" %in% x && "features_to_genome_gtf" %in% x && "values_to_genome_gtf" %in% x))) {
        stop("Indices ", paste(which(y), collapse = ", "), " do not contain 'features', 'origin', 'values_to_genome_gtf' and 'features_to_genome_gtf'.")
    }

    ## lots of checking needed actually, like if to_genome_gtf is a named vector
    # make check: ncbi_data[["features_to_genome_gtf"]]  should only contain 'exon' or 'transcript' # names are value that should match to entries in Feature column

    if (any(y <- sapply(ncbi_data_list, function(x) is.null(names(x[["features_to_genome_gtf"]]))))) {
        stop("Indices ", paste(which(y), collapse = ", "), ":  features_to_genome_gtf has to be a named character vector.")
    }
    if (any(y <- sapply(ncbi_data_list, function(x) !all(x[["features_to_genome_gtf"]] %in% c("exon", "transcript"))))) {
        stop("Indices ", paste(which(y), collapse = ", "), ":  features_to_genome_gtf can only contain 'exon' or 'transcript'.")
    }

    lines_to_genome_and_gtf <- lapply(ncbi_data_list, function(x) {
        # other name for origin (seq name in genome.fa)
        if ("origin_name" %in% names(x)) {
            x[["origin"]] <- x[["origin_name"]]
        }
        features <-
            x[["features"]] %>%
            dplyr::filter(value %in% x[["values_to_genome_gtf"]]) %>%
            dplyr::filter(Feature %in% names(x[["features_to_genome_gtf"]]))

        if (grepl(">", features[,"range"]) || grepl("<", features[,"range"])) {
            sub <- dplyr::filter(features, grepl("<|>", range))
            message("Found ", nrow(sub), " features with '<' or '>' in ranges. Will exclude those.")
            features <- dplyr::filter(features, !grepl("<|>", range))
        }

        # https://kb.10xgenomics.com/hc/en-us/articles/115003327112-How-can-we-add-genes-to-a-reference-package-for-Cell-Ranger-
        lines_to_gtf <- sapply(1:nrow(features), function(y) {
            paste(names(x[["origin"]]), # Chromosome
                  "unknown", # unused
                  x[["features_to_genome_gtf"]][features[y,"Feature",drop=T]], # feature; convert entry in features to
                  as.character(min(as.numeric(unlist(strsplit(strsplit(features[y,"range",drop=T], ",")[[1]], "\\.\\."))))), # range start
                  as.character(max(as.numeric(unlist(strsplit(strsplit(features[y,"range",drop=T], ",")[[1]], "\\.\\."))))), # range end,
                  ".", # unused
                  ifelse(features[y,"complement",drop=T], "-", "+"), # when complement, then to minus strand
                  ".", # unused
                  paste0("gene_id \"", features[y,"value",drop=T], "\"; ", "transcript_id \"", features[y,"value",drop=T], "\";"), # attributes
                  collapse = "\t")
        })

        return(list(lines_to_genome = c(paste0(">", names(x[["origin"]])), splitString(x[["origin"]], n = 60)),
                    lines_to_gtf = lines_to_gtf))
    })


    # think about this
'    if (overwrite && file.exists(genome_file_path) && !append) {
        file.remove(genome_file_path)
    }
    if (overwrite && file.exists(genome_file_path) && !append) {
        file.remove(gtf_file_path)
    }'

    # write files
    vroom::vroom_write_lines(unlist(sapply(lines_to_genome_and_gtf, "[", "lines_to_genome"), use.names = F),
                             genome_file_path,
                             append = append)

    ## gtf header?
    if (append) {
        gtf_header <- NULL
    }
    vroom::vroom_write_lines(c(gtf_header,
                               unlist(sapply(lines_to_genome_and_gtf, "[", "lines_to_gtf"), use.names = F)),
                             gtf_file_path,
                             append = append)
}
