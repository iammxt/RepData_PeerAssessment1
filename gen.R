#!/usr/bin/env Rscript

library(knitr)
setwd('./')
knit2html('PA1_template.Rmd')
browseURL('PA1_template.html')
