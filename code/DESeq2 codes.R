install.packages("BiocManager")
library(DESeq2)
read.csv("C:\Users\F_AKTER\Documents\github\cw_bio708\code\GSE201043_Jima_all_feature_count_formated.csv")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("DESeq2")
install.packages(c("ggplot2", "pheatmap", "RColorBrewer"))
library(DESeq2)
library(ggplot2)
library(pheatmap)
library(RColorBrewer)
counts <-
  "GSE201043_Jima_all_feature_count_formated"
  header = TRUE
  row.names = 1
  check.names = FALSE


head(counts)
dim(counts)
dim(counts)
head(counts)
colnames(counts)
ls()
class(GSE201043_Jima_all_feature_count_formated)
class(data)
class(counts)
class(data)
str(GSE201043_Jima_all_feature_count_formated)
dim(GSE201043_Jima_all_feature_count_formated)
counts <- data
dim (counts)
head(GSE201043_Jima_all_feature_count_formated)
colnames(GSE201043_Jima_all_feature_count_formated)
GSE201043_Jima_all_feature_count_formated <- data[, -1]

rownames(GSE201043_Jima_all_feature_count_formated) <- data$Geneid
dim(count)
head(count)
colnames(count)
counts <- count[, -1]
rownames(counts) <- count$Geneid
dim(counts)
library(DESeq2)

condition <- factor(
  c(
    "LR", "LR", "LR", "LR",
    "LS", "LS", "LS", "LS",
    "HR", "HR", "HR", "HR",
    "HS", "HS", "HS", "HS"
  ),
  levels = c("LR", "LS", "HR", "HS")
)

coldata <- data.frame(condition = condition)
rownames(coldata) <- colnames(counts)
coldata
colnames(counts)
all(rownames(coldata) == colnames(counts))
dds <- DESeqDataSetFromMatrix(
  countData = counts,
  colData = coldata,
  design = ~ condition
)



keep <- rowSums(counts(dds) >= 10) >= 4
dds <- dds[keep, ]

dds <- DESeq(dds)
resultsNames(dds)
res_LR_vs_LS <- results(
  dds,
  contrast = c("condition", "LS", "LR")
)

res_LR_vs_LS <- res_LR_vs_LS[
  order(res_LR_vs_LS$padj),
]


DEG_LR_vs_LS <- subset(
  as.data.frame(res_LR_vs_LS),
  padj < 0.05 & abs(log2FoldChange) >= 1
)



nrow(DEG_LR_vs_LS)


head(DEG_LR_vs_LS, 20)


dim(count)
head(count)
colnames(count)


counts <- count[, -1]
rownames(counts) <- count$Geneid
dim(counts)
library(DESeq2)
condition <- factor(
  c(
    "LR", "LR", "LR", "LR",
    "LS", "LS", "LS", "LS",
    "HR", "HR", "HR", "HR",
    "HS", "HS", "HS", "HS"
  ),
  levels = c("LR", "LS", "HR", "HS")
)

coldata <- data.frame(condition = condition)

rownames(coldata) <- colnames(counts)
coldata


dds <- DESeqDataSetFromMatrix(
  countData = counts,
  colData = coldata,
  design = ~ condition
)


keep <- rowSums(counts(dds) >= 10) >= 4
dds <- dds[keep, ]

dds <- DESeq(dds)
resultsNames(dds)


res_LS_vs_LR <- results(
  dds,
  name = "condition_LS_vs_LR"
)

res_LS_vs_LR <- res_LS_vs_LR[
  order(res_LS_vs_LR$padj),
]

head(res_LS_vs_LR, 20)


DEG_LS_vs_LR <- subset(
  as.data.frame(res_LS_vs_LR),
  !is.na(padj) &
    padj < 0.05 &
    abs(log2FoldChange) >= 1
)

nrow(DEG_LS_vs_LR)
##downregulated genes

down_LS_vs_LR <- subset(
  as.data.frame(res_LS_vs_LR),
  !is.na(padj) &
    padj < 0.05 &
    log2FoldChange <= -1
)

nrow(down_LS_vs_LR)


##top deg
head(DEG_LS_vs_LR, 20)
res_HR_vs_LR <- results(
  dds,
  name = "condition_HR_vs_LR"
)

res_HR_vs_LR <- res_HR_vs_LR[
  order(res_HR_vs_LR$padj),
]


DEG_HR_vs_LR <- subset(
  as.data.frame(res_HR_vs_LR),
  !is.na(padj) &
    padj < 0.05 &
    abs(log2FoldChange) >= 1
)

nrow(DEG_HR_vs_LR)


res_HS_vs_LR <- results(
  dds,
  name = "condition_HS_vs_LR"
)

res_HS_vs_LR <- res_HS_vs_LR[
  order(res_HS_vs_LR$padj),
]



DEG_HS_vs_LR <- subset(
  as.data.frame(res_HS_vs_LR),
  !is.na(padj) &
    padj < 0.05 &
    abs(log2FoldChange) >= 1
)

nrow(DEG_HS_vs_LR)



write.csv(
  DEG_LS_vs_LR,
  "DEG_LS_vs_LR.csv"
)

write.csv(
  DEG_HR_vs_LR,
  "DEG_HR_vs_LR.csv"
)

write.csv(
  DEG_HS_vs_LR,
  "DEG_HS_vs_LR.csv"
)



nrow(DEG_LS_vs_LR)
nrow(DEG_HR_vs_LR)
nrow(DEG_HS_vs_LR)