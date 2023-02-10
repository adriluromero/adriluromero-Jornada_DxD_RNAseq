# **Jornada_DxD_RNAseq**

pipeline for RNA-seq of soil README

Adriana L. Romero-Olivares' protocol for processing RNAseq data of soil

This pipeline is divided into 11 sections. Each section has a filename and a step.

## Hardware Configuration
Ran using New Mexico State University's Discovery HPC Cluster
Operating System: CentOS 7
Scheduler: Slurm 21.08.4


## Build Conda Environments:

File name: Conda_environment_RNA_seq

Step: Building your conda environment for analysis of RNA sequencing data. Download and use the 'rna_seq.yml' file for this step. 

File name: Conda_environment_run_dbcan

Step: Building your conda environment for the '8_Annotate_metatranscriptome' step.

## Section 1

File name: 01_Preparing_files_for_pipeline

Step: Unzipping and merging of sequencing files.

## Section 2

File name: 02_Clean_trim_QC

Step: Cleaning-trimming of raw reads.

## Section 3

File name: 03_SortmeRNA

Step: Preparing files for community analysis and Trinity.

## Section 4

File name: 04_Assembly_de_novo

Step: Assembly with Trinity.

## Section 5

File name: 05_Alignment_bowtie2

Step: Alignment of reads with bowtie2.

## Section 6

File name: 06_Quant_express

Step: Quantification of reads.

## Section 7

File  name: 07_Diff_expression

Step: Differential expression analysis.

## Section 8

File name: 08_Annotate_metatranscriptome

Step: Annotate metatranscriptome.

## Section 9_CAZy

File name: 09_CAZy_Create_annotated_metatranscriptome

Step: Create annotated metatranscriptome for CAZy database.

## Section 9_pfam

File name: 09_pfam_Create_annotated_metatranscriptome

Step: Create annotated metatranscriptome for pfam database.

## Section 10_CAZy

File name: 10_CAZy_last_steps

Step: Repeat steps 5, 6, and 7.

## Section 10_pfam

File name: 10_pfam_last_steps

Step: Repeat steps 5, 6, and 7. 

## Section 11_CAZy

File name: 11_CAZy_merge_files_for_analysis

Step: Merge your output files into one dataframe in Rstudio (Manual curation step).

## Section 11_pfam

File name: 11_pfam_merge_files_for_analysis

Step: Merge your output files into one dataframe in RStudio (Manual curation step).