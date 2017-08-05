# BPS-PortlandWageByIndustry

The table of coefficients that you need from me is in the file "WageGroupProportions_byIndustry.csv".

The code to calculate the proportions of employees in each of the 4 wage groups by industry is the file "Codes4proportionsByIndustry.R". All the Excel files from BLS (Industry-Occupation Matrices for all industries) are in the Temp folder. You can download this repository on your local computer and run the "Codes4proportionsByIndustry.R" file. All files and data required for the output are included in the repository. 

To update these numbers: 
- We need to update the cut-off points for wages based on new BLS data for Portland MSA. Currently they are based on 2016 data. (The file that needs to be replaced is "PDXwages_by_occ.csv".)
- To update the Industry-occupation matrices we have to re-download the Excel files from BLS site (by uncommenting one line in the "Codes4proportionsByIndustry.R" file). Currently we are using 2014 data (latest available).
