Q1. Download the FFIEC’s Home Mortgage Disclosure Act (HMDA) Loan/Application Register (LAR) data for all FDIC secured loans 
at https://ffiec.cfpb.gov/data-publication/snapshot-national-loan-level-dataset/2018. 

Q2. Load the data. Please note, the datafiles do not have column names, which you must add

pacman :: p_load(pacman, tidyverse, dplyr, ggplot2, cowplot, pastecs)
library(stringr)
library(formattable)

#Imported the IL dataset
load("total_dat.Rdata")

q3. Data work:

a. Reduce both data files to only the State your group is exploring. Reduce both data
files to only the State (using State FIPS code) your group is exploring.

b. Append both files into a single data set.

#parts a and b have already been taken care of in the importing aspect of the assignment.

c. Format the FIPS county code digits with leading zeros as necessary. Format the FIPS
county code as 3 digits with leading zeros as necessary.

i. This may be easier to do when reading in the file originally depending on the program
you employ.

ii. You are welcome to make the data type text if that is the only way you can figure it out.

