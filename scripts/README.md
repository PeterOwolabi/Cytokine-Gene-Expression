# Cytokine Gene Expression Analysis

This repository contains the R code for analyzing **IL-6, IL-10, IL-1B and TGF-B1 gene expression** using the $2^{-\Delta\Delta Ct}$ method. It includes the full workflow from raw $Ct$ values to statistical grouping and publication-quality visualization.

## Analysis Workflow
1. **Normalization**: Calculation of $\Delta Ct$ ($Ct_{target} - Ct_{reference}$) and $\Delta\Delta Ct$ using the a group as a calibrator.
2. **Relative Quantitation (RQ)**: Transformation of data into Fold Change ($2^{-\Delta\Delta Ct}$).
3. **Statistical Analysis**: 
   - **One-way ANOVA** to test for significant differences across the groups.
   - **Tukey HSD Post-hoc Test** for pairwise comparisons.
4. **Significance Grouping**: Automatic assignment of compact letter displays (CLD) using `multcompView`.
5. **Visualization**: A themed bar plot with error bars (Standard Error) and significance letters.

## Required R Packages
To run this analysis, you will need the following libraries:
- `readxl`: For data import.
- `dplyr`: For data manipulation.
- `rstatix`: For descriptive statistics.
- `multcompView`: For significance letter assignments.
- `ggplot2`: For high-quality plotting.

## Quick Start
1. Ensure your raw data is saved as `IL_1B MREC.xlsx` in the project directory.
2. Open the R script and update the file path to point to your local directory.
3. Run the script to generate the `group_summary` table and the final plot.
