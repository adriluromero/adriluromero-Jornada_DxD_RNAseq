#Jornada_DxD_RNAseq

#Adriana L. Romero-Olivares' protocol for processing RNAseq data of soil
#Pipeline ran by Andrea Lopez
#Section 11
#File name: 11_pfam_merge_files_for_analysis
#Step: Merge your output files into one dataframe in RStudio (Manual curation step). 

#In this script, you will: 

# 1. Download the files you will use from the discovery into your local PC.
# 2. RStudio: Clean the working environment and load the packages you need.
# 3. RStudio: Merge the pfam interspace files. 
# 4. RStudio: Merge the pfam underspace files. 

#1. Download the files to use from the discovery into your local PC.

#Example:

#/fs1/project/egcc/RNAseq_drought_disturbance/pfam/All_inter_hmmscan_pfam.out
#/fs1/project/egcc/RNAseq_drought_disturbance/pfam/All_under_hmmscan_pfam.out

#/fs1/project/egcc/RNAseq_drought_disturbance/pfam/estimate_abundance/align_estimate_inter/Trinity.gene.counts.matrix  #rename to Inter.Trinity.gene.counts.matrix after downloading.
#/fs1/project/egcc/RNAseq_drought_disturbance/pfam/estimate_abundance/align_estimate_under/Trinity.gene.counts.matrix  #rename to Under.Trinity.gene.counts.matrix after downloading.

#To save the 'Inter.Trinity.gene.counts.matrix' and 'Under.Trinity.gene.counts.matrix' as a csv file, copy and paste each file separately into excel then save as .csv

#2. RStudio: Clean the working environment and load the packages you need in RStudio.

#Clean your working environment.
rm(list=ls())

#Load the packages you'll be using
library("utils")
library("stringr")
library("dplyr")

####Begin with pfam interspace####

#3. RStudio: Merge the pfam interspace files.

#Import the gene matrix saved as a .csv from your local PC. 

inter_gene_matrix <- read.csv("C:/Users/lopan/Desktop/RNA_files_for_R/pfam/Inter.Trinity.gene.counts.matrix.csv")

#Import the .out file into RStudio. 
#This is a large file formatted distinctly so it will require some additional steps to open it in RStudio.

#Give it the file location

file_loc <- "C:/Users/lopan/Desktop/RNA_files_for_R/pfam/All_inter_hmmscan_pfam.out"
con <- file(file_loc, "r", blocking = FALSE) 
my_lines <- readLines(con)
close(con)

#Get rid of unnecessary information

my_lines[1]
my_lines[3]

#Remove the first and 3rd line 

my_lines <- my_lines[-3]
my_lines <- my_lines[-1]

headers <- my_lines[1]  #save headers to use later
my_lines <- my_lines[-1] #remove header row

split_num <- 22 #the number of columns in the data with everything else collapsing down to col 23

#for each line in the file

for (i in 1:length(my_lines)){
  #split each line of the file on whitespace 
  temp <- strsplit(my_lines[i], " +")
  #collect the first 'split_num' chunks as data
  data_cols <- temp[[1]][1:split_num]
  #obtain the rest of the chunks
  comment <- temp[[1]][(split_num + 1):length(temp[[1]])]
  comment <- paste(comment, collapse = " ") #put the spaces back in to these remaining chunks 
  #if another character should replace commas add here
  comment <- str_replace_all(comment, ',', '') #remove all commas so the csv doesn't get messed up
  #collapse both the data and the comment seperated by comma for csv
  my_lines[i] <- paste(c(data_cols, comment), collapse = ",")
}

#This will take a few minutes. 

#Save the file as csv

save_loc <- "C:/Users/lopan/Desktop/RNA_files_for_R/pfam/All_inter_hmmscan_pfam_mod.csv"
con <- file(save_loc, "w")
writeLines(my_lines, con = con)
close(con)

#Read in the newly saved .csv file 

inter_pfam_hmmscan <- read.csv(file = "C:/Users/lopan/Desktop/RNA_files_for_R/pfam/All_inter_hmmscan_pfam_mod.csv", header = F)

print(headers)

#Use printed headers to rename col names

edited_names <- c("target_name", "accession", "tlen", "query_name", "accession", "qlen", "E-value", "score", "bias", "#", "of", "c-Evalue", "i-Evalue", "score",  "bias", "from", "to", "from", "to", "from", "to", "acc", "description_of_target")
colnames(inter_pfam_hmmscan) <- edited_names

#The 'inter_pfam_hmmscan' file has extra information in the contig name.
#Remove the *_i from each contig so it can match with the contigs name in the 'inter_gene_matrix'. 

inter_pfam_hmmscan$query_name2 <- gsub("_i.*","",inter_pfam_hmmscan$query_name)

#Select the three columns we are interested in keeping.

inter_pfam_hmmscan_new <- inter_pfam_hmmscan %>%select(target_name,query_name2,description_of_target)  

#Rename the column 

inter_pfam_hmmscan_new <- inter_pfam_hmmscan_new %>%
  rename("query_name" = "query_name2")

#Only keep one distinct value for each query.name. This keeps the first hit.

inter_pfam_hmmscan_new2 <- inter_pfam_hmmscan_new %>% distinct(query_name, .keep_all = TRUE)

#In 'inter_gene_matrix', rename the first column to "query_name" for merging.

colnames(inter_gene_matrix)[1] <- "query_name"

#Now do a left join to merge dataframes

inter_pfam_merged <- left_join(inter_gene_matrix, inter_pfam_hmmscan_new2, by = "query_name")

#Let's move the target_name column to the right of the query_name column

inter_pfam_merged <- inter_pfam_merged %>% relocate(target_name, .after = query_name)

#Rename columns 3 through 12 based on treatment name.
#You find this information on the "reps_inter_pfam.txt' file from the '6_quant_express' step. 

col_names <- list('query_name', 'target_name', 'GSF3123.301CI','GSF3123.306CI',
                  'GSF3123.307CI','GSF3123.313DrI','GSF3123.314DrI','GSF3123.320DrI',
                  'GSF3123.327DiI','GSF3123.329DiI','GSF3123.332DxDI','GSF3123.339DxDI','description_of_target')

colnames(inter_pfam_merged) <- c(col_names)

#Export as csv

write.csv(inter_pfam_merged, "C:/Users/lopan/Desktop/RNA_files_for_R/pfam/merged/inter_pfam_merged.csv", row.names = FALSE)


####Continue with pfam underspace####

#Clean your working environment.
rm(list=ls())

#4. RStudio: Merge the pfam underspace files.

#Import the gene matrix saved as a .csv from your local PC.

under_gene_matrix <- read.csv("C:/Users/lopan/Desktop/RNA_files_for_R/pfam/Under.Trinity.gene.counts.matrix.csv")

#Import the .out file into RStudio. 
#This is a large file formatted distinctly so it will require some additional steps to open it in RStudio.

#give it the file location 

file_loc <- "C:/Users/lopan/Desktop/RNA_files_for_R/pfam/All_under_hmmscan_pfam.out"
con <- file(file_loc, "r", blocking = FALSE) 
my_lines <- readLines(con)
close(con)

#Get rid of unnecessary information

my_lines[1]
my_lines[3]

#Remove the first and 3rd line 

my_lines <- my_lines[-3]
my_lines <- my_lines[-1]

headers <- my_lines[1] #save headers to use later
my_lines <- my_lines[-1] #remove header row

split_num <- 22 #the number of columns in the data with everything else collapsing down to col 23

#for each line in the file

for (i in 1:length(my_lines)){
  #split each line of the file on whitespace 
  temp <- strsplit(my_lines[i], " +")
  #collect the first 'split_num' chunks as data
  data_cols <- temp[[1]][1:split_num]
  #obtain the rest of the chunks
  comment <- temp[[1]][(split_num + 1):length(temp[[1]])]
  comment <- paste(comment, collapse = " ") #put the spaces back in to these remaining chunks 
  #if another character should replace commas add here
  comment <- str_replace_all(comment, ',', '') #remove all commas so the csv doesn't get messed up
  #collapse both the data and the comment seperated by comma for csv
  my_lines[i] <- paste(c(data_cols, comment), collapse = ",")
}

#This will take a few minutes. 

#Save the file as csv

save_loc <- "C:/Users/lopan/Desktop/RNA_files_for_R/pfam/All_under_hmmscan_pfam_mod.csv"
con <- file(save_loc, "w")
writeLines(my_lines, con = con)
close(con)

#Read in the newly saved csv

under_pfam_hmmscan <- read.csv(file = "C:/Users/lopan/Desktop/RNA_files_for_R/pfam/All_under_hmmscan_pfam_mod.csv", header = F)

print(headers)

#use printed headers to rename col names

edited_names <- c("target_name", "accession", "tlen", "query_name", "accession", "qlen", "E-value", "score", "bias", "#", "of", "c-Evalue", "i-Evalue", "score",  "bias", "from", "to", "from", "to", "from", "to", "acc", "description_of_target")

colnames(under_pfam_hmmscan) <- edited_names

#The 'under_pfam_hmmscan' file has extra information in the contig name.
#Remove the *_i from each contig so it can match with the contigs name in the 'under_gene_matrix'. 

under_pfam_hmmscan$query_name2 <- gsub("_i.*","",under_pfam_hmmscan$query_name)

#Select the three columns we are interested in keeping.

under_pfam_hmmscan_new <- under_pfam_hmmscan %>%select(target_name,query_name2,description_of_target)  

#Rename column. 

under_pfam_hmmscan_new <- under_pfam_hmmscan_new %>%
  rename("query_name" = "query_name2")

#Only keep one distinct value for each query.name. This keeps the first hit. 

under_pfam_hmmscan_new2 <- under_pfam_hmmscan_new %>% distinct(query_name, .keep_all = TRUE)

#In 'under_gene_matrix', rename the first column to "query_name".

colnames(under_gene_matrix)[1] <- "query_name"

#Now do a left join to merge data frames.

under_pfam_merged <- left_join(under_gene_matrix, under_pfam_hmmscan_new2, by = "query_name")

#Let's move the target_name column to the right of the query_name column.

under_pfam_merged <- under_pfam_merged %>% relocate(target_name, .after = query_name)

#Rename columns 3 through 12 based on treatment name. 
#You find this information on the "reps_under_pfam.txt' file from the '6_quant_express' step.  

col_names <- list('query_name', 'target_name','GSF3123.305CU','GSF3123.306CU',
                  'GSF3123.307CU','GSF3123.311DrU','GSF3123.313DrU','GSF3123.314DrU',
                  'GSF3123.322DiU','GSF3123.324DiU','GSF3123.333DxDU','GSF3123.338DxDU','description_of_target')

colnames(under_pfam_merged) <- c(col_names)

#Export as csv

write.csv(under_pfam_merged, "C:/Users/lopan/Desktop/RNA_files_for_R/pfam/merged/under_pfam_merged.csv", row.names = FALSE)