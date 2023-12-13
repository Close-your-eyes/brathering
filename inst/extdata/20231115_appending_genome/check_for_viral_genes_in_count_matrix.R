
# /Users/christopher.skopnik/Downloads/NR_JK_053/outs/filtered_feature_bc_matrix.h5
# /Users/christopher.skopnik/Downloads/Pat4_rep1_blood/outs/filtered_feature_bc_matrix.h5
viral_mod_053 <- Seurat::Read10X_h5("/Users/christopher.skopnik/Downloads/NR_JK_053_viral_mod/filtered_feature_bc_matrix.h5") # raw
hg38_053 <- Seurat::Read10X_h5("/Users/christopher.skopnik/Downloads/NR_JK_053_hg38/filtered_feature_bc_matrix.h5") # raw


viral_mod_004 <- Seurat::Read10X_h5("/Users/christopher.skopnik/Downloads/NR_JK_004_viral_mod/raw_feature_bc_matrix.h5")
hg38_004 <- Seurat::Read10X_h5("/Users/christopher.skopnik/Downloads/NR_JK_004_hg38/raw_feature_bc_matrix.h5")


viral_mod_046 <- Seurat::Read10X_h5("/Users/christopher.skopnik/Downloads/NR_JK_046_viral_mod/raw_feature_bc_matrix.h5")
hg38_046 <- Seurat::Read10X_h5("/Users/christopher.skopnik/Downloads/NR_JK_046_hg38/raw_feature_bc_matrix.h5")


viral_mod_053_sub <- viral_mod_053[which(grepl("_CDS$|_mRNA$|CD81|CD84", rownames(viral_mod_053))),]
hg38_053_sub <- hg38_053[which(grepl("CD81$|CD84$", rownames(hg38_053))),]
df_viral_mod_053_sub <- stack(rowSums(as.matrix(viral_mod_053_sub)))
df_hg38_053_sub <- stack(rowSums(as.matrix(hg38_053_sub)))



viral_mod_004_sub <- viral_mod_004[which(grepl("_CDS$|_mRNA$|CD81|CD84", rownames(viral_mod_004))),]
hg38_004_sub <- hg38_004[which(grepl("CD81$|CD84$", rownames(hg38_004))),]
df_viral_mod_004_sub <- stack(rowSums(as.matrix(viral_mod_004_sub)))
df_hg38_004_sub <- stack(rowSums(as.matrix(hg38_004_sub)))



viral_mod_046_sub <- viral_mod_046[which(grepl("_CDS$|_mRNA$|CD81|CD84", rownames(viral_mod_046))),]
hg38_046_sub <- hg38_046[which(grepl("CD81$|CD84$", rownames(hg38_046))),]
df_viral_mod_046_sub <- stack(rowSums(as.matrix(viral_mod_046_sub)))
df_hg38_046_sub <- stack(rowSums(as.matrix(hg38_046_sub)))



## positive control for EBV? from pawel

# 
EBV_ctrl_raw <- Seurat::Read10X_h5("/Volumes/4TB_Backup_Geraet/EBV_ctrl_viral_mod/raw_feature_bc_matrix.h5") # raw
EBV_ctrl_filter <- Seurat::Read10X_h5("/Volumes/4TB_Backup_Geraet/EBV_ctrl_viral_mod/filtered_feature_bc_matrix.h5") # raw

EBV_ctrl_raw_sub <- EBV_ctrl_raw[which(grepl("_CDS$|_mRNA$", rownames(EBV_ctrl_raw))),]
EBV_ctrl_filter_sub <- EBV_ctrl_filter[which(grepl("_CDS$|_mRNA$", rownames(EBV_ctrl_filter))),]
df_EBV_ctrl_raw_sub <- stack(rowSums(as.matrix(EBV_ctrl_raw_sub)))
df_EBV_ctrl_filter_sub <- stack(rowSums(as.matrix(EBV_ctrl_filter_sub)))






## check genome and gtf file again


test2 <- Seurat::Read10X("/Volumes/CMS_SSD_2TB/R_scRNAseq/2022_SLE_LN_full_sorts/data/Sequencing_data/NR_JK_053/filtered_feature_bc_matrix")
test2 <- test2[["Gene Expression"]]


test3 <- as.matrix(test2[which(grepl("CD8", rownames(test2))),])
rowSums(test3)


test <- igsc::read_fasta("/Volumes/CMS_SSD_2TB/20231115_appending_genome/viral_genomes/NM_004356.4_CD81_mRNA.txt")

nchar(test)
