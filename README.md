# APOE2 analysis
The GitHub repository includes several folders with different functionalities:

"apoe2_paper_stats_volume":

Contains R files that compute and visualize descriptive statistics based on brain region volume data.
Requires extracting data from the "individual_label_statistics.zip" file.

"mice_with_symbol":

Includes an R file and an input file to add symbols to RNA normalized count.

"multi_cca_git":

Contains the "r_reader_connectome.R" file, which reads individual connectome, RNA, and phenotype data.
Generates three output files: "connectivity.rda", "RNA_data.rda", and "response.rda".
These files serve as input for SMCCA (Sparse Multiple Canonical Correlation Analysis).

"apoe2_rna_sig_age", "apoe2_rna_sig_sex", and "winding_multi_cca":

Include the SMCCA files "multicca.R".
Each folder has its own purpose and data specifications.
"winding_multi_cca" folder also includes additional behavioral data, such as winding number.

"winding_multi_cca":

Contains "d1_5_anovas.R" and "winding_repeated_day1-5.R" files.
Responsible for descriptive statistics on different behavioral probe data.

"brainconnsetting.R" and "bootcca_multi_cca.R":

Visualize 3D brain results.
Compute the 95% bootstrap confidence interval after running "multicca.R".

"multi_cca_volcano":

Includes two folders, "apoe2_rna_sig_age" and "apoe2_rna_sig_sex".
These folders contain R files to plot volcano plots after SMCCA filtering.
The input for these files is derived from the results of "apoe2_rna_sig_sex" and "apoe2_rna_sig_age", specifically the "RNA_result_ordered.xlsx" file.
