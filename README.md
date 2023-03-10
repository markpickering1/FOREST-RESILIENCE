# FOREST-RESILIENCE Analysis

Repository containing the code related to producing the climate and kndvi datasets for the resilience datasets as well as running the resilience-biodiversity analysis.
The repository code and structure is a work in progress that will develop as the analysis progreses.
The repository is mainly aimed at containing the code used in the analysis. As such an initialising script sets up the local directory extensions in order to export any data produced into an external directory.


## Structure

There are several code directories:

- `code` : this is an area for reasonably up to date work that is organised and functioning as scripts that can be used for current analysis. It is cleaner and contains four main directories:
0) main area for initialisation scripts and master files
1) scripts for downloading and cleaning the input datasets (e.g. from GEE, ERA5)
2) scripts for combining the different data sources
3) scripts for analysis and visualising the data

- `sandbox` : This is an area for quick messy studies and versions of code that are not finalised or currently works in progress. It can be used for experimentation and testing. Once code is more established and working it can be addd to one of the other directories

- `legacy` : This is an area for old versions of code that we want to keep a copy of but are no longer active lines of investigation or analysis (but might be taken in the future). For example early studies such as comparison with TCI, that are compete but will not form the major lines of analysis.

- `TBO` : This is an area for code that is yet to be organised into the analysis chain. For example, they may be copies of scripts


## Input data sources
TBC - write the sources of the different input datasets


## To run the code

Initialise the bash code by first running the script `code/main/initialise_bash.sh`
Similarly, initialise the R code by running the script `code/main/initialise_R.R` at the start of each R file
Initialise the plotting environment by running the script `code/main/initialise_figs.R` at the start of each R plotting script