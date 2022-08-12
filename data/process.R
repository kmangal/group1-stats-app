# Pre-processing for TNPSC Group 1 01/2019 Data

rm(list = ls())

library(tidyverse)
library(data.table)

# ====================================================

# Application data
app <- fread('data/raw/application_01_2019.csv')
names(app) <- tolower(names(app))

#app$blind <- as.integer(app$pwdvh == "YES")
#app$deaf <- as.integer(app$pwdhh == "YES")
#app$ortho <- as.integer(app$pwdlh == "YES")
#app$mental <- as.integer(app$pwdmh == "YES")

#app <- app %>% mutate(disability.sum = blind+ deaf + ortho + mental)

#app$disability <- case_when(
#  (app$blind == 1) & (app$disability.sum == 1) ~ 'blind',
#  (app$deaf == 1) & (app$disability.sum == 1) ~ 'deaf',
#  (app$ortho == 1) & (app$disability.sum == 1) ~ 'ortho',
#  (app$mental == 1) | (app$disability.sum > 1) ~ 'multda',
#  TRUE ~ 'none'
#)

app$disability <- as.integer(app$disability == "YES")

app <- app %>% rename(community.full = community)

app$community <- case_when(
  (app$community.full == "OTHERS") ~ "OTHERS",
  TRUE ~ str_extract(app$community.full, "(?<=\\()(.*?)(?=\\))")
)

app$pstm <- pmax(as.integer(app$eqpstm == "YES"), as.integer(app$uglpstm == "YES"), as.integer(app$ugpstm == "YES"))
app$exservice <- as.integer(app$exser == "YES")
app$widow <- as.integer(app$widow == "YES")

app$highest.qual <- ""
app$highest.qual <- ifelse(app$ugdegree != "", "UG", app$highest.qual)
app$highest.qual <- ifelse(app$ugldegree != "", "UG", app$highest.qual)
app$highest.qual <- ifelse(app$eqdegmajsub != "", "UG", app$highest.qual)
app$highest.qual <- ifelse(app$pgdegree != "", "PG", app$highest.qual)
app$highest.qual <- ifelse(app$ipgdegree != "", "PG", app$highest.qual)
app$highest.qual <- ifelse(app$mphilmajor != "", "MPhil/PhD", app$highest.qual)
app$highest.qual <- ifelse(app$phdmajor != "", "MPhil/PhD", app$highest.qual)

table(app$highest.qual)

app$age <- 2019 - as.integer(app$dob)

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
  select(regno, gender, age, nativedistrict, highest.qual,
         pstm, exservice, widow, disability, govtemp, 
         wrote.prelim, total.prelim, wrote.main, total.main, selected)

saveRDS(df, 'data/clean/merged_01_2019.Rds')

