---
title: "Codebook automation for Punjab employment dataset"
author: "Sanya Sareen"
date: "16/09/2021"
output: html_document
##Edited by
author: "Apoorva Limaye"
date: "07/12/2021"
---
install.packages(c("tidyverse","dplyr","stringr", "knitr", "stargazer", "readxl", "kableExtra","lubridate", "fastDummies", "here", "DT", "Hmisc", "broom", "xtable", "foreign", "data.table", "RStata", "janitor", "codebook", "labelled", "rgdal", "tmap", "sp","haven"))
##```{r setup, include=FALSE}
##knitr::opts_chunk$set(echo = TRUE)
##```
# Clear workspace
rm(list = ls())

metadata(pb_employ)$name <- ""
# Setup workspace
options(scipen = 999)
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

# Load packages (easy loading through pacman)
pacman::p_load(tidyverse, dplyr, stringr, knitr, stargazer, readxl, kableExtra, lubridate, fastDummies, here, DT, Hmisc, broom, xtable, foreign, data.table, RStata, janitor, codebook, labelled, rgdal, tmap, sp, haven)
## =====
# Set paths
root <- here() # Gets the root of the GitHub repo automatically without having to specify user ID
datadir <- paste0(root, "C:/Users/Apoorva Limaye/Desktop/Punjab_Employment/data/")
dtadir <- paste0(datadir,'dta/')
```

```{r processdata, include=FALSE}
############### READ DATA ##################

# Reading csv file
   job_provider <- read_dta(paste0(dtadir,"job_provider_data/","job_provider_appended_dta.dta"),sep="|", na.strings = c(" ", "","0"))

################# DATA CLEANING ################
# Dropping PII columns, changing variable types (integer, factor with level labels, logical etc.), renaming variables for clarity, reordering variables etc.
# Omitted for sample script

################################################

```{r codebookmetadata, include = FALSE}

# Adding metadata

  ## Variable labels
      var_label(secc_urban) <- list(
        ahl_tin = "Abridged House List - Temporary Identification Number",
        new_uid = "New UID variable constructed based on AHL TIN construction instructions provided in SECC 2011 documentation",
        tin_npr = "National Population Register (NPR) survey slip number or Tax Identification Number",
        ahl_hh_id = "AHL Household ID",
        new_hhid = "New HHID variable constructed based on instructions provided in SECC 2011 documentation",
        st_code = "State Code",
        dt_code_2001 = "District Code (Census 2001)",
        dt_code_mdds = "District Code (Census 2011)",
        dt_name_mdds = "District Name (Census 2011)",
        sdt_code_mdds = "Sub-District Code (Census 2011)",
        sdt_name_mdds = "Sub-District Name (Census 2011)",
        tehsil_code = "Tehsil Code",
        tv_code_secc = "Town/Village Code (Census 2001)",
        tv_code_mdds = "Town/Village Code (Census 2011)",
        tv_name_mdds = "Town/Village Name (Census 2011)",
        wardid = "Ward ID",
        grampanchayat = "Gram Panchayat (Only for Rural residents)",
        ahlblockno = "AHL Block Number",
        ahlsubblockno = "AHL Sub-Block Number",
        ahlslnohhd = "AHL House Number",
        slnomember = "Household Member Number",
        relation = "Relation to Head of Household",
        dob = "Year of Birth",
        genderid = "Gender",
        mstatusid = "Marital Status",
        pin_code = "Pin Code",
        caste_group = "Caste Group",
        incomesource_urban = "Income Source (only for Urban residents)",
        rural_urban = "Rural/Urban resident",
        hoh = "Head of Household?",
        age = "Age",
        i_1 = "Households without shelter",
        i_2 = "Destitute, living on alms",
        i_3 = "Manual scavenger families",
        i_4 = "Primitive tribal groups",
        i_5 = "Legally released bonded labour",
        d_1 = "Households with one (or less) room, kuccha walls and/or kuccha roof",
        d_2 = "No adult member in household between age 18 and age 59",
        d_3 = "Female-headed household with no adult male member between ages 16 and 59",
        d_4 = "Households with a differently abled member and with no other able-bodied adult member",
        d_5 = "SC/ST households",
        d_7 = "Landless households deriving a major part of their income from manual labour"
      )
  
  ## Dataset description
  metadata(secc_urban)$name <- "SECC 2011 - Haryana (Urban)"
  metadata(secc_urban)$description <- "Data from the Socio Economic Caste Census 2011 for Urban residents of the state of Haryana."
  
```


```{r codebookgen, echo = FALSE}

# Variables constructed/added to SECC dataset later
    new_vars <- c("new_uid", "new_hhid", "dt_code_mdds", "dt_name_mdds", "sdt_code_mdds", "sdt_name_mdds", "tv_name_mdds", "age")

# Categorizing variables by type for easier sorting
    variable_type <- c("ID", "ID", "ID", "ID", "ID", "Location", "Location", "Location", "Location", "Location", "Location", "Location", "Location", "Location", "Location", "Location", "Location", "Location", "Location", "Location", "Location", "Demographics", "Demographics", "Demographics", "Demographics", "Demographics", "Demographics", "Demographics", "Demographics", "Income Indicators", "Income Indicators", "Income Indicators", "Income Indicators", "Income Indicators", "Income Indicators", "Income Indicators", "Income Indicators", "Income Indicators", "Income Indicators", "Income Indicators", "Income Indicators", "Misc", "Misc")

   
# Using codebook command to create metatable datatable
    codebook_urban <- codebook_table(secc_urban)
      
# Urban sample size
    urban_size <- count(secc_urban)[1,1]
      
# Classifying columns as empty if missing # is same as sample size
    codebook_urban$empty <- ifelse(codebook_urban$n_missing == urban_size, 1, 0)
      
# Deleting unused metadata columns
    codebook_urban <- codebook_urban %>%
      select(-c("whitespace", "count")) %>%
        
# Creating column to indicate new variables
     mutate(constructed = ifelse(grepl(paste0(new_vars, collapse="|"), codebook_urban$name), 1, 0)) %>%
        
# Rounding decimal places
     mutate_if(is.numeric, round, 4)
      
# Combining variable type vector with codebook
     codebook_urban <- cbind(variable_type, codebook_urban)
    
# reordering
     codebook_urban <- codebook_urban[,c(1:3, 18, 4:17)]
    
# Metadata for some confusing variables
     var_label(codebook_urban) <- list(
        constructed = "Created/added later; Not in original dataset",
        ordered = "If factor variables: Are levels ordered?",
        value_labels = "Levels for all factor variables",
        n_missing = "# observations in which variable is missing",
        complete_rate = "# observations with variable present / Sample size",
        top_counts = "Gives observation counts for most frequent factor levels"
        )
  
rm(new_vars, variable_type)
```

Metadata and codebooks for Urban data from the SECC 2011. Only includes the poorest 40% of the population as measured by deprivation criteria.

# Codebooks {.tabset .tabset-fade}

## Sample
`r count(secc_urban)` individuals
<br>
`r secc_urban %>% group_by(ahl_hh_id) %>% summarise(n = n()) %>% count()` families

## Codebook
```{r codebookdisp_urb, echo = FALSE}

# Generate urban codebook using datatable command
datatable(codebook_urban, rownames = FALSE, filter = "top",
          colnames = c("Created/added - Not in original dataset" = "constructed",
                       "ordered_factor" = "ordered"),
          extensions = c('FixedColumns'),
          options = list(pageLength = 5, 
                         scrollX = TRUE,
                         fixedColumns = list(leftColumns = 2))) %>%
  formatStyle('variable_type', backgroundColor = 'lightslategrey', fontWeight = 'bold') %>%
  formatStyle('name', backgroundColor = 'lightgrey', fontWeight = 'bold') %>%
  formatStyle('complete_rate', backgroundColor = styleInterval(c(0.9, 1), c('yellow', '', '')))

```
