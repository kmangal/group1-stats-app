# Pre-processing for TNPSC Group 1 01/2019 Data

rm(list = ls())

library(tidyverse)
library(data.table)

# ====================================================

# Application data
app <- fread('data/raw/application_01_2019.csv')
names(app) <- tolower(names(app))
#saveRDS(app, 'data/clean/application_01_2019.Rds')

# Selection data
selection <- fread('data/raw/selection_01_2019.csv')
names(selection) <- tolower(names(selection))
selection <- selection %>% rename(selcat = `selection category`)
#saveRDS(selection, 'data/clean/selection_01_2019.Rds')

# Prelim marks data
prelim <- fread('data/raw/marks_01_2019_prelims.csv')
names(prelim) <- tolower(names(prelim))
prelim <- prelim %>% rename(total.prelim = total)
#saveRDS(prelim, 'data/clean/marks_01_2019_prelims.Rds')

# Main marks data
main <- fread('data/raw/marks_01_2019_mains.csv')
names(main) <- tolower(names(main))
main <- main %>% rename(total.main = total)
#saveRDS(main, 'data/clean/marks_01_2019_mains.Rds')


# Merged data
df <- app %>% left_join(prelim %>% select(regno, total.prelim), by = 'regno')
df <- df %>% mutate(wrote.prelim = !is.na(total.prelim))

df <- df %>% left_join(main %>% select(regno, total.main), by = 'regno')
df <- df %>% mutate(wrote.main = !is.na(total.main))

df <- df %>% left_join(selection %>% select(regno, selcat), by = 'regno')
df <- df %>% mutate(selected = as.integer(!is.na(selcat)))

df <- df %>% 
  select(regno, gender, dob, nativedistrict, religion, community, exser, widow, govtemp, 
         wrote.prelim, total.prelim, wrote.main, total.main, selected)

saveRDS(df, 'data/clean/merged_01_2019.Rds')

