# CHiRP

Four files are included. Data must be independently obtained from CRSP.

ticker_scripts.R: contains many general-use functions referenced by other scripts. Does nothing on its own.

runtime_code_final.R: runs analysis for a year of data and saves the output. Should be run in Yearly directory (i.e folder containing folders for each year)

plotting_code_final.R: loads, performs analysis, and plots data. (Mostly for analyzing aggregate data)

per_year_plotting.R: supplementary plotting & analysis; loops through years. (Mostly for analyzing each year individually)


Each year's data should be organized into a folder labeled 20XX; all year folders should then be placed in a single folder.

