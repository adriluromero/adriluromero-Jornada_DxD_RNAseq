pipeline for RNA-seq of soil README

Adriana L. Romero-Olivares' protocol for processing RNAseq data of soil

This pipeline is divided into Section A and B, and Section 1 through 11. Each section has a filename and a step.

Sections A and B are for building the conda environments used in this pipeline.

Section A

File name: A_rna_seq_conda_environment

Step: Building your conda environment for analysis of rna sequencing data.

Download and use the 'RNA_Seq.yml' file for this step. 

Section B

File name: B_run_dbcan_conda_environment

Step: Building your conda environment for the '8_Annotate_metatranscriptome' step.

Section 1

File name: 1_Preparing_files_for_pipeline

Step: Unzipping and merging of sequencing files.

Section 2

File name: 2_Clean_trim_QC

Step: Cleaning-trimming of raw reads.

Section 3

File name: 3_SortmeRNA

Step: Preparing files for community analysis and Trinity.

Section 4

File name: 4_Assembly_de_novo

Step: Assembly with Trinity.

Section 5

File name: 5_Alignment_bowtie2

Step: Alignment of reads with bowtie2.

Section 6

File name: 6_Quant_express

Step: Quantification of reads.

Section 7

File  name: 7_Diff_expression

Step: Differential expression analysis.

Section 8

File name: 8_Annotate_metatranscriptome

Step: Annotate metatranscriptome.

Section 9_CAZy

File name: 9_CAZy_Create_annotated_metatranscriptome

Step: Create annotated metatranscriptome for CAZy database.

Section 9_pfam

File name: 9_pfam_Create_annotated_metatranscriptome

Step: Create annotated metatranscriptome for pfam database.

Section 10_CAZy

File name: 10_CAZy_last_steps

Step: Repeat steps 5, 6, and 7.

Section 10_pfam

File name: 10_pfam_last_steps

Step: Repeat steps 5, 6, and 7. 

Section 11_CAZy

File name: 11_CAZy_merge_files_for_analysis

Step: Merge your output files into one dataframe (Manual curation step).

Section 11_pfam

File name: 11_pfam_merge_files_for_analysis

Step: Merge your output files into one dataframe in RStudio (Manual curation step).