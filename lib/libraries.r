library(readr)
library(dplyr)
library(tidyr)
library(knitr)
library(rvest)
library(gsubfn)
library(tmap)
library(shiny)
library(tibble)
library(gghighlight)


library(ggplot2)
library(ggforce)
library(ggimage)
library(scales)
library(mosaic)

library(shinyWidgets)

library(tidyverse)
library(readxl)

library(gridExtra)
library(ggnewscale)
library(tidyquant)
library(quantmod)
library(RColorBrewer)

library(foreach) 
library(compiler)

library(anytime) 
library(lubridate) 

library(plotly)
library(quantmod)

library(httr)


options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")