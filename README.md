# Isotopic Pattern Generator (High Resolution)

This program is written in R and calculates the isotopic pattern using the elemental composition. 

## Prerequisites

The code requires the R package 'data.table' and 'testthat' to be installed

## Input

The code takes as input the elemental composition, i.e., number of atoms of carbon, hydrogen, nitrogen, oxygen, sulphur and number of isotope positions wanted. The input file is 'Input.txt' which is an ASCII tab delimited text file with atoms of carbon, hydrogen, nitrogen, oxygen, sulphur and number of isotopes in that order.

## Output

The output is written to 'output.txt' in the 'results' directory which has the isotope position, abundance and relative abundance of each peak.

## Unit Tests

The unit tests for this code is in 'test_code.R' in the 'tests' directory and can be performed by running

```r
test_file(file.path("tests","test_code.R"))
```
## R Shiny app

Check the link for R Shiny app
[Isotopic Pattern Predictor](https://nandhinidev.shinyapps.io/isopatapp/)
