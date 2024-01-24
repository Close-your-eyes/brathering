path <- "/Volumes/4TB_Backup_Geraet/2019_SLE_LN_fastq_files_and_hg38_mapped/hg38_CR7_wo_introns/GEX/mapped_with_bamboozled_BAM_fastq_pat_id0"
target_path <- "/Volumes/CMS_SSD_2TB/R_scRNAseq/2019_SLE_LN/data/Sequencing_data/hg38_CR7_wo_introns/GEX_bamboozled"

outs_paths <- fs::dir_ls(path = path, type = "directory", recurse = 1, regexp = "outs")
h5_files <- unname(unlist(purrr::map(outs_paths, fs::dir_ls, regexp = "\\matrix.h5$")))
h5_files_split <- strsplit(h5_files, "/")
target_files <- file.path(target_path, sapply(h5_files_split, "[", 8), sapply(h5_files_split, "[", 9), sapply(h5_files_split, "[", 10))
fs::dir_create(dirname(target_files))

file.copy(h5_files, target_files)