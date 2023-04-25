library(lintr)
library(styler)

lintr::use_lintr(type = "tidyverse")

lintr::lint("script.R")
styler::style_file("script.R")
lintr::lint("script.R")
