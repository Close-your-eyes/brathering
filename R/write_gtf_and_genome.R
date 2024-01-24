write_gtf_and_genome_for_cellranger <- function(data,
                                                genome_file_name = "genome.fa",
                                                gtf_file_name = "genes.gtf",
                                                save_path = NULL,
                                                gtf_header = c("##description: made with write_gtf_and_genome_for_cellranger function from https://github.com/Close-your-eyes/igsc", "##provider: CMS", "##conctact: vonskopnik@pm.me", "##format: gtf", paste0("##date: ", Sys.Date())),
                                                features_to_become_exon = c("CDS"),
                                                other_features_to_write = NULL,
                                                append = F,
                                                overwrite = F) {

    # https://www.10xgenomics.com/support/software/cell-ranger/latest/tutorials/cr-tutorial-mr
    # https://www.ensembl.org/info/website/upload/gff.html
    # https://support.10xgenomics.com/single-cell-gene-expression/software/pipelines/latest/advanced/references#multi
    # https://kb.10xgenomics.com/hc/en-us/articles/115003327112-How-can-we-add-genes-to-a-reference-package-for-Cell-Ranger-

    # cellranger count only uses exons - define somewhere which features become exons: features_to_become_exon

    # only two things are needed for every "chromosome" to be added: the sequence (origin) and the feature data frame which needs at least 3 columns (Feature, range, value) ?

    # write a minimal example for that

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
    data <- list(ncbi_data, ncbi_data2)'

    if (!is.list(data) || is.null(names(data))) {
        stop("data has to be a named list.")
    }
    if (anyDuplicated(names(data)) != 0) {
        stop("names of data have to be unique!")
    }
    if (any(temp <- !purrr::map_lgl(data, function(x) all(c("features", "origin") %in% names(x))))) {
        stop("every list entry of data has to contain 'features' and 'origin' at least. check: ", paste(names(which(temp)), collapse = ","))
    }
    if (any(temp <- !purrr::map_lgl(data, function(x) is.data.frame(x[["features"]])))) {
        stop("every features entry has to be a data frame. check: ", paste(names(which(temp)), collapse = ","))
    }
    if (any(temp <- !purrr::map_lgl(data, function(x) all(c("range", "complement", "Feature", "value") %in% names(x[["features"]]))))) {
        stop("every features data frame has to have columns named: 'range', 'complement', 'Feature', 'value'. check: ", paste(names(which(temp)), collapse = ","))
    }
    if (any(temp <- !purrr::map_lgl(data, function(x) all(c(length(x[["origin"]]) == 1, is.character(x[["origin"]])))))) {
        stop("every origin entry has to be a character of length 1. check: ", paste(names(which(temp)), collapse = ","))
    }
    if (!is.character(features_to_become_exon)) {
        stop("features_to_become_exon has to be a character vector.")
    }
    if (!is.null(other_features_to_write) && !is.character(other_features_to_write)) {
        stop("other_features_to_write has to be a character vector.")
    }
    if (!is.null(other_features_to_write)) {
        if (any(other_features_to_write %in% features_to_become_exon)) {
            rm <- other_features_to_write[which(other_features_to_write %in% features_to_become_exon)]
            stop(paste(rm, collapse = ", "), " from other_features_to_write were also in features_to_become_exon. Please remove.")
        }
    }
    if (is.null(save_path)) {
        stop("Please provide a save_path.")
    }
    if (append && overwrite) {
        stop("append (= attach new lines to existing file) and overwrite (= replacing an existing file), both set to TRUE does not make sense. select one only, please.")
    }
    if (genome_file_name != "genome.fa") {
        message("for 10X pipeline (mkref and cellranger) the genome_file may be preferably named 'genome.fa")
    }
    if (gtf_file_name != "genes.gtf") {
        message("for 10X pipeline (mkref and cellranger) the gtf_file may be preferably named 'genes.gtf")
    }

    genome_file_path <- file.path(save_path, genome_file_name)
    gtf_file_path <- file.path(save_path, gtf_file_name)
    if (file.exists(gtf_file_path) && !append && !overwrite) {
        stop(gtf_file_path, " exists. It must be appended (append = T) or overwritten (overwrite = T) or another file needs to be written.")
    }
    if (file.exists(genome_file_path) && !append && !overwrite) {
        stop(genome_file_path, " exists. It must be appended (append = T) or overwritten (overwrite = T) or another file needs to be written.")
    }

    # remove < > from range column of features data frames
    rm_temp <- F
    for (i in purrr::map_dfr(data, `[[`, "features")[["range"]]) {
        if (grepl("<|>", i)) {
            message("< and/or > detected in range column of at least one features data frame. Will remove those. The indicate uncertainty of feature boundaries.")
            rm_temp <- T
            break
        }
    }
    if (rm_temp) {
        for (i in names(data)) {
            data[[i]][["features"]][["range"]] <- gsub("<|>", "", data[[i]][["features"]][["range"]])
        }
    }
    # check for numbers, dots and comma only in range column
    for (i in purrr::map_dfr(data, `[[`, "features")[["range"]]) {
        if (!grepl("^[0-9.,]+$", i)) {
            stop("At least one range contains other symbols than digits and dots. This should not be. Please check.")
        }
    }
    # check if complement column is logical
    if (any(temp <- !purrr::map_lgl(data, function(x) all(is.logical(x[["features"]][["complement"]]))))) {
        stop("complement columns of features data frames have to be logical (TRUE or FALSE only). check: ", paste(names(which(temp)), collapse = ","))
    }


    dir.create(save_path, showWarnings = F, recursive = T)
    feat_select <- unique(c(features_to_become_exon, other_features_to_write))
    lines_to_genome_and_gtf <- purrr::map(setNames(names(data), names(data)), function(x) {

        # allow for other column names, and check above
        features <-
            data[[x]][["features"]] %>%
            dplyr::filter(Feature %in% feat_select)
        features$Feature[which(features$Feature %in% features_to_become_exon)] <- "exon"

        # notify when 0 rows remained
        if (nrow(features) > 0) {
            lines_to_gtf <- sapply(1:nrow(features), function(y) {
                paste(x, # Chromosome
                      "unknown", # unused
                      features[y,"Feature",drop=T], # feature; convert entry in features to
                      as.character(min(as.numeric(unlist(strsplit(strsplit(features[y,"range",drop=T], ",")[[1]], "\\.\\."))))), # range start
                      as.character(max(as.numeric(unlist(strsplit(strsplit(features[y,"range",drop=T], ",")[[1]], "\\.\\."))))), # range end,
                      ".", # unused
                      ifelse(features[y,"complement",drop=T], "-", "+"), # when complement, then to minus strand
                      ".", # unused
                      paste0("gene_id \"", gsub("\\.", "", make.names(features[y,"value",drop=T])), "\"; ", "transcript_id \"", gsub("\\.", "", make.names(features[y,"value",drop=T])), "\";"), # attributes
                      sep = "\t")
            })

            return(list(lines_to_genome = c(paste0(">", x), splitString(data[[x]][["origin"]], n = 60)),
                        lines_to_gtf = lines_to_gtf))
        } else {
            message("zero rows in features data frame remained for ", x, ". check 'features_to_become_exon' and 'other_features_to_write' !?")
            return(NULL)
        }
    }, .progress = T)




    if (overwrite && file.exists(genome_file_path)) {
        file.remove(genome_file_path)
    }
    if (overwrite && file.exists(gtf_file_path)) {
        file.remove(gtf_file_path)
    }

    # write files
    vroom::vroom_write_lines(unlist(sapply(lines_to_genome_and_gtf, "[", "lines_to_genome"), use.names = F),
                             genome_file_path,
                             append = append)

    ## gtf header?
    if (append) {
        message("Since gtf file is appended, the header lines provided are ignored.")
        gtf_header <- NULL
    }
    vroom::vroom_write_lines(c(gtf_header,
                               unlist(sapply(lines_to_genome_and_gtf, "[", "lines_to_gtf"), use.names = F)),
                             gtf_file_path,
                             append = append)
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
