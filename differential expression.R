
##load DESeq2
library(DESeq2)
library(tidyverse)

###control vs disturbance only INTER pathogenicity 

mycounts <- read.csv("countData_inter_CvsDi.csv")
metadata <- read.csv("colData_inter_CvsDi.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_inter <- results(dds, tidy=TRUE)
res_inter <- as_tibble(res_inter)
res_inter
print(res_inter, n = 3000) ##can skip to avoid output


write.table(res_inter, file="DESeq2_inter_pathogenicity_CvsDi.csv", row.names=F, sep=",")


###control vs drought only INTER pathogenicity 

mycounts <- read.csv("countData_inter_CvsDr.csv")
metadata <- read.csv("colData_inter_CvsDr.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_inter <- results(dds, tidy=TRUE)
res_inter <- as_tibble(res_inter)
res_inter
print(res_inter, n = 3000)


write.table(res_inter, file="DESeq2_inter_pathogenicity_CvsDr.csv", row.names=F, sep=",")


###control vs DxDx only INTER pathogenicity 

mycounts <- read.csv("countData_inter_CvsDxD.csv")
metadata <- read.csv("colData_inter_CvsDxD.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_inter <- results(dds, tidy=TRUE)
res_inter <- as_tibble(res_inter)
res_inter
print(res_inter, n = 3000)


write.table(res_inter, file="DESeq2_inter_pathogenicity_CvsDxD.csv", row.names=F, sep=",")



###control vs disturbance only UNDER pathogenicity 

mycounts <- read.csv("countData_under_CvsDi.csv")
metadata <- read.csv("colData_under_CvsDi.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_under <- results(dds, tidy=TRUE)
res_under <- as_tibble(res_under)
res_under
print(res_under, n = 3000)


write.table(res_under, file="DESeq2_under_pathogenicity_CvsDi.csv", row.names=F, sep=",")


###control vs drought only UNDER pathogenicity 

mycounts <- read.csv("countData_under_CvsDr.csv")
metadata <- read.csv("colData_under_CvsDr.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_under <- results(dds, tidy=TRUE)
res_under <- as_tibble(res_under)
res_under
print(res_under, n = 3000)


write.table(res_under, file="DESeq2_under_pathogenicity_CvsDr.csv", row.names=F, sep=",")


###control vs DxDx only UNDER pathogenicity 

mycounts <- read.csv("countData_under_CvsDxD.csv")
metadata <- read.csv("colData_under_CvsDxD.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_under <- results(dds, tidy=TRUE)
res_under <- as_tibble(res_under)
res_under
print(res_under, n = 3000)


write.table(res_under, file="DESeq2_under_pathogenicity_CvsDxD.csv", row.names=F, sep=",")



####  ALL pfam inter CvsDi

mycounts <- read.csv("inter_pfam_ALL_CvsDi.csv")
metadata <- read.csv("colData_inter_CvsDi.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_inter_ALL <- results(dds, tidy=TRUE)
res_inter_ALL <- as_tibble(res_inter_ALL)
res_inter_ALL
#print(res_inter_ALL, n = 3000)


write.table(res_inter_ALL, file="DESeq2_inter_ALL_CvsDi.csv", row.names=F, sep=",")


####  ALL pfam inter CvsDr

mycounts <- read.csv("inter_pfam_ALL_CvsDr.csv")
metadata <- read.csv("colData_inter_CvsDr.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_inter_ALL <- results(dds, tidy=TRUE)
res_inter_ALL <- as_tibble(res_inter_ALL)
res_inter_ALL
#print(res_inter_ALL, n = 3000)


write.table(res_inter_ALL, file="DESeq2_inter_ALL_CvsDr.csv", row.names=F, sep=",")


####ALL pfam inter CvsDxD

mycounts <- read.csv("inter_pfam_ALL_CvsDxD.csv")
metadata <- read.csv("colData_inter_CvsDxD.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_inter_ALL <- results(dds, tidy=TRUE)
res_inter_ALL <- as_tibble(res_inter_ALL)
res_inter_ALL
#print(res_inter_ALL, n = 3000)


write.table(res_inter_ALL, file="DESeq2_inter_ALL_CvsDxD.csv", row.names=F, sep=",")



#### ALL pfam under CvsDi

mycounts <- read.csv("under_pfam_ALL_CvsDi.csv")
metadata <- read.csv("colData_under_CvsDi.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_under_ALL <- results(dds, tidy=TRUE)
res_under_ALL <- as_tibble(res_under_ALL)
res_under_ALL
#print(res_under_ALL, n = 3000)


write.table(res_under_ALL, file="DESeq2_under_ALL_CvsDi.csv", row.names=F, sep=",")


#### ALL pfam under CvsDr

mycounts <- read.csv("under_pfam_ALL_CvsDr.csv")
metadata <- read.csv("colData_under_CvsDr.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_under_ALL <- results(dds, tidy=TRUE)
res_under_ALL <- as_tibble(res_under_ALL)
res_under_ALL
#print(res_under_ALL, n = 3000)


write.table(res_under_ALL, file="DESeq2_under_ALL_CvsDr.csv", row.names=F, sep=",")


#### ALL pfam under CvsDxD

mycounts <- read.csv("under_pfam_ALL_CvsDxD.csv")
metadata <- read.csv("colData_under_CvsDxD.csv")

mycounts <- as.data.frame(mycounts)
metadata <- as.data.frame(metadata)
head(mycounts)
head(metadata)
class(mycounts)
class(metadata)

dds <- DESeqDataSetFromMatrix(countData=mycounts, 
                              colData=metadata, 
                              design=~treatment, 
                              tidy=TRUE)
dds

dds <- DESeq(dds)

res_under_ALL <- results(dds, tidy=TRUE)
res_under_ALL <- as_tibble(res_under_ALL)
res_under_ALL
#print(res_under_ALL, n = 3000)


write.table(res_under_ALL, file="DESeq2_under_ALL_CvsDxD.csv", row.names=F, sep=",")



##### Volcano plots INTER ALL PFAM CvsDi
library(ggplot2)
tmp <- read.csv("differentially_expressed_inter_ALL_pfam_with_namesCvsDi.csv")

# remove rows that contain NA values
de <- tmp[complete.cases(tmp), ]

# The basic scatter plot: x is "log2FoldChange", y is "pvalue"
ggplot(data=de, aes(x=log2FoldChange, y=pvalue)) + geom_point()

# Convert directly in the aes()
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point()

# Add more simple "theme"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point() + theme_minimal()

# Add vertical lines for log2FoldChange thresholds, and one horizontal line for the p-value threshold 
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

# The significantly differentially expressed genes are the ones found in the upper-left and upper-right corners.
# Add a column to the data frame to specify if they are UP- or DOWN- regulated (log2FoldChange respectively positive or negative)

# add a column of NAs
de$diffexpressed <- "NO"
# if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP" 
de$diffexpressed[de$log2FoldChange > 0.6 & de$pvalue < 0.05] <- "UP"
# if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
de$diffexpressed[de$log2FoldChange < -0.6 & de$pvalue < 0.05] <- "DOWN"

# Re-plot but this time color the points with "diffexpressed"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed)) + geom_point() + theme_minimal()

# Add lines as before...
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

## Change point color 

# 1. by default, it is assigned to the categories in an alphabetical order):
p3 <- p2 + scale_color_manual(values=c("blue", "black", "red"))

# 2. to automate a bit: ceate a named vector: the values are the colors to be used, the names are the categories they will be assigned to:
mycolors <- c("blue", "red", "black")
names(mycolors) <- c("DOWN", "UP", "NO")
p3 <- p2 + scale_colour_manual(values = mycolors)

# Now write down the name of genes beside the points...
# Create a new column "delabel" to de, that will contain the name of genes differentially expressed (NA in case they are not)
de$delabel <- NA
de$delabel[de$diffexpressed != "NO"] <- de$target_name[de$diffexpressed != "NO"]

ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) + 
  geom_point() + 
  theme_minimal() +
  geom_text()

# Finally, we can organize the labels nicely using the "ggrepel" package and the geom_text_repel() function
# load library
library(ggrepel)
# plot adding up all layers we have seen so far
Inter_ALL_pfam_CvsDi <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) +
  geom_point() + 
  theme_minimal() +
  geom_text_repel(max.overlaps = getOption("ggrepel.max.overlaps", default = 20)) +
  scale_color_manual(values=c("blue", "black", "red")) +
  geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")
Inter_ALL_pfam_CvsDi

jpeg("Inter_ALL_pfam_CvsDi.jpeg",height=10,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(Inter_ALL_pfam_CvsDi)
dev.off()


##### Volcano plots INTER ALL PFAM CvsDr
library(ggplot2)
tmp <- read.csv("differentially_expressed_inter_ALL_pfam_with_namesCvsDr.csv")

# remove rows that contain NA values
de <- tmp[complete.cases(tmp), ]

# The basic scatter plot: x is "log2FoldChange", y is "pvalue"
ggplot(data=de, aes(x=log2FoldChange, y=pvalue)) + geom_point()

# Convert directly in the aes()
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point()

# Add more simple "theme"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point() + theme_minimal()

# Add vertical lines for log2FoldChange thresholds, and one horizontal line for the p-value threshold 
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

# The significantly differentially expressed genes are the ones found in the upper-left and upper-right corners.
# Add a column to the data frame to specify if they are UP- or DOWN- regulated (log2FoldChange respectively positive or negative)

# add a column of NAs
de$diffexpressed <- "NO"
# if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP" 
de$diffexpressed[de$log2FoldChange > 0.6 & de$pvalue < 0.05] <- "UP"
# if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
de$diffexpressed[de$log2FoldChange < -0.6 & de$pvalue < 0.05] <- "DOWN"

# Re-plot but this time color the points with "diffexpressed"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed)) + geom_point() + theme_minimal()

# Add lines as before...
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

## Change point color 

# 1. by default, it is assigned to the categories in an alphabetical order):
p3 <- p2 + scale_color_manual(values=c("blue", "black", "red"))

# 2. to automate a bit: ceate a named vector: the values are the colors to be used, the names are the categories they will be assigned to:
mycolors <- c("blue", "red", "black")
names(mycolors) <- c("DOWN", "UP", "NO")
p3 <- p2 + scale_colour_manual(values = mycolors)

# Now write down the name of genes beside the points...
# Create a new column "delabel" to de, that will contain the name of genes differentially expressed (NA in case they are not)
de$delabel <- NA
de$delabel[de$diffexpressed != "NO"] <- de$target_name[de$diffexpressed != "NO"]

ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) + 
  geom_point() + 
  theme_minimal() +
  geom_text()

# Finally, we can organize the labels nicely using the "ggrepel" package and the geom_text_repel() function
# load library
library(ggrepel)
# plot adding up all layers we have seen so far
Inter_ALL_pfam_CvsDr <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) +
  geom_point() + 
  theme_minimal() +
  geom_text_repel(max.overlaps = getOption("ggrepel.max.overlaps", default = 10)) +
  scale_color_manual(values=c("blue", "black", "red")) +
  geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")
Inter_ALL_pfam_CvsDr

jpeg("Inter_ALL_pfam_CvsDr.jpeg",height=10,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(Inter_ALL_pfam_CvsDr)
dev.off()



##### Volcano plots INTER ALL PFAM CvsDxD
library(ggplot2)
tmp <- read.csv("differentially_expressed_inter_ALL_pfam_with_namesCvsDxD.csv")

# remove rows that contain NA values
de <- tmp[complete.cases(tmp), ]

# The basic scatter plot: x is "log2FoldChange", y is "pvalue"
ggplot(data=de, aes(x=log2FoldChange, y=pvalue)) + geom_point()

# Convert directly in the aes()
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point()

# Add more simple "theme"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point() + theme_minimal()

# Add vertical lines for log2FoldChange thresholds, and one horizontal line for the p-value threshold 
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

# The significantly differentially expressed genes are the ones found in the upper-left and upper-right corners.
# Add a column to the data frame to specify if they are UP- or DOWN- regulated (log2FoldChange respectively positive or negative)

# add a column of NAs
de$diffexpressed <- "NO"
# if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP" 
de$diffexpressed[de$log2FoldChange > 0.6 & de$pvalue < 0.05] <- "UP"
# if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
de$diffexpressed[de$log2FoldChange < -0.6 & de$pvalue < 0.05] <- "DOWN"

# Re-plot but this time color the points with "diffexpressed"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed)) + geom_point() + theme_minimal()

# Add lines as before...
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

## Change point color 

# 1. by default, it is assigned to the categories in an alphabetical order):
p3 <- p2 + scale_color_manual(values=c("blue", "black", "red"))

# 2. to automate a bit: ceate a named vector: the values are the colors to be used, the names are the categories they will be assigned to:
mycolors <- c("blue", "red", "black")
names(mycolors) <- c("DOWN", "UP", "NO")
p3 <- p2 + scale_colour_manual(values = mycolors)

# Now write down the name of genes beside the points...
# Create a new column "delabel" to de, that will contain the name of genes differentially expressed (NA in case they are not)
de$delabel <- NA
de$delabel[de$diffexpressed != "NO"] <- de$target_name[de$diffexpressed != "NO"]

ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) + 
  geom_point() + 
  theme_minimal() +
  geom_text()

# Finally, we can organize the labels nicely using the "ggrepel" package and the geom_text_repel() function
# load library
library(ggrepel)
# plot adding up all layers we have seen so far
Inter_ALL_pfam_CvsDxD <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) +
  geom_point() + 
  theme_minimal() +
  geom_text_repel(max.overlaps = getOption("ggrepel.max.overlaps", default = 20)) +
  scale_color_manual(values=c("blue", "black", "red")) +
  geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")
Inter_ALL_pfam_CvsDxD

jpeg("Inter_ALL_pfam_CvsDxD.jpeg",height=10,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(Inter_ALL_pfam_CvsDxD)
dev.off()







##### Volcano plots UNDER ALL PFAM CvsDi
library(ggplot2)
tmp <- read.csv("differentially_expressed_under_ALL_pfam_with_namesCvsDi.csv")

# remove rows that contain NA values
de <- tmp[complete.cases(tmp), ]

# The basic scatter plot: x is "log2FoldChange", y is "pvalue"
ggplot(data=de, aes(x=log2FoldChange, y=pvalue)) + geom_point()

# Convert directly in the aes()
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point()

# Add more simple "theme"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point() + theme_minimal()

# Add vertical lines for log2FoldChange thresholds, and one horizontal line for the p-value threshold 
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

# The significantly differentially expressed genes are the ones found in the upper-left and upper-right corners.
# Add a column to the data frame to specify if they are UP- or DOWN- regulated (log2FoldChange respectively positive or negative)

# add a column of NAs
de$diffexpressed <- "NO"
# if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP" 
de$diffexpressed[de$log2FoldChange > 0.6 & de$pvalue < 0.05] <- "UP"
# if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
de$diffexpressed[de$log2FoldChange < -0.6 & de$pvalue < 0.05] <- "DOWN"

# Re-plot but this time color the points with "diffexpressed"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed)) + geom_point() + theme_minimal()

# Add lines as before...
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

## Change point color 

# 1. by default, it is assigned to the categories in an alphabetical order):
p3 <- p2 + scale_color_manual(values=c("blue", "black", "red"))

# 2. to automate a bit: ceate a named vector: the values are the colors to be used, the names are the categories they will be assigned to:
mycolors <- c("blue", "red", "black")
names(mycolors) <- c("DOWN", "UP", "NO")
p3 <- p2 + scale_colour_manual(values = mycolors)

# Now write down the name of genes beside the points...
# Create a new column "delabel" to de, that will contain the name of genes differentially expressed (NA in case they are not)
de$delabel <- NA
de$delabel[de$diffexpressed != "NO"] <- de$target_name[de$diffexpressed != "NO"]

ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) + 
  geom_point() + 
  theme_minimal() +
  geom_text()

# Finally, we can organize the labels nicely using the "ggrepel" package and the geom_text_repel() function
# load library
library(ggrepel)
# plot adding up all layers we have seen so far
Under_ALL_pfam_CvsDi <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) +
  geom_point() + 
  theme_minimal() +
  geom_text_repel(max.overlaps = getOption("ggrepel.max.overlaps", default = 20)) +
  scale_color_manual(values=c("blue", "black", "red")) +
  geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")
Under_ALL_pfam_CvsDi

jpeg("Under_ALL_pfam_CvsDi.jpeg",height=10,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(Under_ALL_pfam_CvsDi)
dev.off()


##### Volcano plots INTER ALL PFAM CvsDr
library(ggplot2)
tmp <- read.csv("differentially_expressed_under_ALL_pfam_with_namesCvsDr.csv")

# remove rows that contain NA values
de <- tmp[complete.cases(tmp), ]

# The basic scatter plot: x is "log2FoldChange", y is "pvalue"
ggplot(data=de, aes(x=log2FoldChange, y=pvalue)) + geom_point()

# Convert directly in the aes()
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point()

# Add more simple "theme"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point() + theme_minimal()

# Add vertical lines for log2FoldChange thresholds, and one horizontal line for the p-value threshold 
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

# The significantly differentially expressed genes are the ones found in the upper-left and upper-right corners.
# Add a column to the data frame to specify if they are UP- or DOWN- regulated (log2FoldChange respectively positive or negative)

# add a column of NAs
de$diffexpressed <- "NO"
# if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP" 
de$diffexpressed[de$log2FoldChange > 0.6 & de$pvalue < 0.05] <- "UP"
# if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
de$diffexpressed[de$log2FoldChange < -0.6 & de$pvalue < 0.05] <- "DOWN"

# Re-plot but this time color the points with "diffexpressed"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed)) + geom_point() + theme_minimal()

# Add lines as before...
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

## Change point color 

# 1. by default, it is assigned to the categories in an alphabetical order):
p3 <- p2 + scale_color_manual(values=c("blue", "black", "red"))

# 2. to automate a bit: ceate a named vector: the values are the colors to be used, the names are the categories they will be assigned to:
mycolors <- c("blue", "red", "black")
names(mycolors) <- c("DOWN", "UP", "NO")
p3 <- p2 + scale_colour_manual(values = mycolors)

# Now write down the name of genes beside the points...
# Create a new column "delabel" to de, that will contain the name of genes differentially expressed (NA in case they are not)
de$delabel <- NA
de$delabel[de$diffexpressed != "NO"] <- de$target_name[de$diffexpressed != "NO"]

ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) + 
  geom_point() + 
  theme_minimal() +
  geom_text()

# Finally, we can organize the labels nicely using the "ggrepel" package and the geom_text_repel() function
# load library
library(ggrepel)
# plot adding up all layers we have seen so far
Under_ALL_pfam_CvsDr <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) +
  geom_point() + 
  theme_minimal() +
  geom_text_repel(max.overlaps = getOption("ggrepel.max.overlaps", default = 10)) +
  scale_color_manual(values=c("blue", "black", "red")) +
  geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")
Under_ALL_pfam_CvsDr

jpeg("Under_ALL_pfam_CvsDr.jpeg",height=10,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(Under_ALL_pfam_CvsDr)
dev.off()



##### Volcano plots INTER ALL PFAM CvsDxD
library(ggplot2)
tmp <- read.csv("differentially_expressed_under_ALL_pfam_with_namesCvsDxD.csv")

# remove rows that contain NA values
de <- tmp[complete.cases(tmp), ]

# The basic scatter plot: x is "log2FoldChange", y is "pvalue"
ggplot(data=de, aes(x=log2FoldChange, y=pvalue)) + geom_point()

# Convert directly in the aes()
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point()

# Add more simple "theme"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue))) + geom_point() + theme_minimal()

# Add vertical lines for log2FoldChange thresholds, and one horizontal line for the p-value threshold 
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

# The significantly differentially expressed genes are the ones found in the upper-left and upper-right corners.
# Add a column to the data frame to specify if they are UP- or DOWN- regulated (log2FoldChange respectively positive or negative)

# add a column of NAs
de$diffexpressed <- "NO"
# if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP" 
de$diffexpressed[de$log2FoldChange > 0.6 & de$pvalue < 0.05] <- "UP"
# if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
de$diffexpressed[de$log2FoldChange < -0.6 & de$pvalue < 0.05] <- "DOWN"

# Re-plot but this time color the points with "diffexpressed"
p <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed)) + geom_point() + theme_minimal()

# Add lines as before...
p2 <- p + geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")

## Change point color 

# 1. by default, it is assigned to the categories in an alphabetical order):
p3 <- p2 + scale_color_manual(values=c("blue", "black", "red"))

# 2. to automate a bit: ceate a named vector: the values are the colors to be used, the names are the categories they will be assigned to:
mycolors <- c("blue", "red", "black")
names(mycolors) <- c("DOWN", "UP", "NO")
p3 <- p2 + scale_colour_manual(values = mycolors)

# Now write down the name of genes beside the points...
# Create a new column "delabel" to de, that will contain the name of genes differentially expressed (NA in case they are not)
de$delabel <- NA
de$delabel[de$diffexpressed != "NO"] <- de$target_name[de$diffexpressed != "NO"]

ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) + 
  geom_point() + 
  theme_minimal() +
  geom_text()

# Finally, we can organize the labels nicely using the "ggrepel" package and the geom_text_repel() function
# load library
library(ggrepel)
# plot adding up all layers we have seen so far
Under_ALL_pfam_CvsDxD <- ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) +
  geom_point() + 
  theme_minimal() +
  geom_text_repel(max.overlaps = getOption("ggrepel.max.overlaps", default = 20)) +
  scale_color_manual(values=c("blue", "black", "red")) +
  geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")
Under_ALL_pfam_CvsDxD

jpeg("Under_ALL_pfam_CvsDxD.jpeg",height=10,width=10,units = 'in', res = 600)#height in inches, resolution 600 dpi
plot(Under_ALL_pfam_CvsDxD)
dev.off()




