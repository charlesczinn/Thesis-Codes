# Thesis-Codes
Various codes used for data cleaning, processing, and analysis for Master’s thesis at the University of Utah. CSV files are used in the scripts.

Dependent_Variable_Alterations.R
- Taking raw incident data from the Anti-Defamation League (ADL) then cleaning, geocoding, cleaning, and joining with county spatial data.

nlp_thesis_processing.ipynb
- Using ADL incident descriptions to identify events included in repository due to change in methods after October 7, 2023.
- Flags events as being tied to these new methods.
- Pre-processing and post-processing done in Dependent_Variable_Alterations.R

Independent_Variable_Processing.R
- Cleaning data for inlcusion in dataset as independent variables
- Data gathered from U.S. Census Bureau ACS, USDA, and the Southern Poverty Law Center (SPLC)
