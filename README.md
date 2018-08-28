# MScProject
Code and words for my MSc project.

## A note on excecution:
Should you find yourself in the uneviable position of needing to re-run my code, I hope the comments included and layout make it trivial to do so. In any case:

* All Data, Code and Results relevant to my final results can be found in corresponding Final_Pipeline folders within these folders.
* `setwd()` should automatically adjust to wherever you've cloned this repository to, as long as you've remembered to install `here`.
* Please run the numbered scripts in order (1-4) before attempting to run any plotting scripts. 
* Experimental data is currently read from a series of .csvs in the Run_X series of folders. This was set up before I found out about the wide range of packages that can extract data directly from Excel worksheets. Should you need to re-use any of the scripts contained here, I would recommend using a function from this package in place of the existing for loop. 

## R packages used:
```
dplyr
tidyr
tibble
ggplot2
growthcurver
gridBase
gridExtra
here
stringr
growthcurver
gridBase
gridExtra
ggpubr
viridis
magrittr
data.table
forcats
```
