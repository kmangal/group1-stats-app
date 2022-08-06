# Pre-processing for TNPSC Group 1 01/2019 Data

library(data.table)


# Application data
app <- fread('data/raw/application_01_2019.csv')
saveRDS(app, 'data/clean/application_01_2019.Rds')

# Selection data
selection <- fread('data/raw/selection_01_2019.csv')
saveRDS(selection, 'data/clean/selection_01_2019.Rds')

# Prelim marks data
prelim <- fread('data/raw/marks_01_2019_prelims.csv')
saveRDS(prelim, 'data/clean/marks_01_2019_prelims.Rds')

# Main marks data
prelim <- fread('data/raw/marks_01_2019_mains.csv')
saveRDS(prelim, 'data/clean/marks_01_2019_mains.Rds')
