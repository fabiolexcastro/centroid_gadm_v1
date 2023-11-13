

### Alliance Bioversity - CIAT
### Autor: Fabio Castro - Llanos 
### Target: Get the coordinates from centroids (SHP)
### November 2023

### Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, crayon, remotes, rgeos, gtools, glue, ggspatial)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# remotes::install_github("dickoa/rhdx")
library(rhdx)

### Configuration connection ---------------------------------------------
set_rhdx_config(hdx_site = "prod")
get_rhdx_config()

### Function to use ------------------------------------------------------
download.east <- function(iso, cnt){
  
  # iso <- 'uga'
  # cnt <- 'Uganda'
  
  cat('... To download: ', iso, '\t')
  lsts <- search_datasets(cnt, rows = 3)
  dtst <- pluck(lsts, 1)
  rsrc <- get_resource(dtst, 2)
  
  cat('Put into the folder\t')
  dir_create(glue('./zip/{iso}'))
  dir_create(glue('./shp/{iso}'))
  read_resource(rsrc, download_folder = glue('./zip/{iso}'))
  
  cat('To unzip\t')
  file <- as.character(dir_ls(glue('./zip/{iso}'), regexp = '.zip$'))
  unzip(zipfile = file, exdir = glue('./shp/{iso}'))
  fles <- as.character(dir_ls(glue('./shp/{iso}'), regexp = '.shp$'))
  return(fles)
  
}

coordnts <- function(shp, lvl){
  
  # shp <- shp1
  # lvl <- 'ADM1_EN'
  
  # To calculate the centroid
  crd <- shp %>% st_centroid() %>% st_coordinates() %>% as_tibble() 
  nms <- shp %>% pull(lvl)
  crd <- crd %>% mutate(name = nms)
  colnames(crd) <- c('Lon', 'Lat', lvl)
  crd <- relocate(crd, 3, 2, 1)
  return(crd)
  
}

### To apply the function ------------------------------------------------

# East Africa -------------------------------------------------------------

# Uganda ------------------------------------------------------------------
iso <- 'uga'
cnt <- 'Uganda'

## To download
fles.uga <- download.east(iso = iso, cnt = cnt)
shp1.uga <- grep('adm1', fles.uga, value = T) %>% st_read()
shp2.uga <- grep('adm2', fles.uga, value = T) %>% st_read()

## To calculate the centroids / table
cnt1.uga <- coordnts(shp = shp1.uga, lvl = 'ADM1_EN')
cnt2.uga <- coordnts(shp = shp2.uga, lvl = 'ADM2_EN')

cnt1.uga <- mutate(cnt1.uga, country = 'UGA')
cnt2.uga <- mutate(cnt2.uga, country = 'UGA')

# To write the files as csv 
dir.create('./tbl/centroids', recursive = TRUE)
write.csv(cnt1.uga, './tbl/centroids/uga_cnt_adm1.csv', row.names = FALSE)
write.csv(cnt2.uga, './tbl/centroids/uga_cnt_adm2.csv', row.names = FALSE)

# Kenya -------------------------------------------------------------------

iso <- 'ken'
cnt <- 'Kenya'

## To download
fles.ken <- download.east(iso = iso, cnt = cnt)
shp1.ken <- grep('adm1', fles.ken, value = T) %>% st_read()
shp2.ken <- grep('adm2', fles.ken, value = T) %>% st_read() %>% st_make_valid()

## To calculate the centroids / table
cnt1.ken <- coordnts(shp = shp1.ken, lvl = 'ADM1_EN') %>% mutate(country = 'KEN')
cnt2.ken <- coordnts(shp = shp2.ken, lvl = 'ADM2_EN') %>% mutate(country = 'KEN')

# To write the files as csv 
write.csv(cnt1.ken, './tbl/centroids/ken_cnt_adm1.csv', row.names = FALSE)
write.csv(cnt2.ken, './tbl/centroids/ken_cnt_adm2.csv', row.names = FALSE)

# Wests Africa -------------------------------------------------------------

# Cameroon ----------------------------------------------------------------

# Download manually from: https://data.humdata.org/dataset/cod-ab-cmr
iso <- 'cmr'
cnt <- 'Cameroon'

fles.cmr <- dir_ls('./shp/cmr', regexp = '.shp$')
shp1.cmr <- grep('adm1', fles.cmr, value = T) %>% st_read()
shp2.cmr <- grep('adm2', fles.cmr, value = T) %>% st_read() %>% st_make_valid()

## To calculate the centroids / table
cnt1.cmr <- coordnts(shp = shp1.cmr, lvl = 'ADM1_EN') %>% mutate(iso = 'CMR')
cnt2.cmr <- coordnts(shp = shp2.cmr, lvl = 'ADM2_FR') %>% mutate(iso = 'CMR')

# To write the files as csv 
write.csv(cnt1.cmr, './tbl/centroids/cmr_cnt_adm1.csv', row.names = FALSE)
write.csv(cnt2.cmr, './tbl/centroids/cmr_cnt_adm2.csv', row.names = FALSE)

# Nigeria -----------------------------------------------------------------

# Download manually from: https://data.humdata.org/dataset/cod-ab-nga
iso <- 'nga'
cnt <- 'Nigeria'

## To download
fles.nga <- dir_ls('./shp/nga', regexp = '.shp$')
shp1.nga <- grep('adm1', fles.nga, value = T) %>% st_read()
shp2.nga <- grep('adm2', fles.nga, value = T) %>% st_read() %>% st_make_valid()

## To calculate the centroids / table
cnt1.nga <- coordnts(shp = shp1.nga, lvl = 'ADM1_EN') %>% mutate(iso = 'NGA')
cnt2.nga <- coordnts(shp = shp2.nga, lvl = 'ADM2_EN') %>% mutate(iso = 'NGA')

# To write the files as csv 
write.csv(cnt1.nga, './tbl/centroids/nga_cnt_adm1.csv', row.names = FALSE)
write.csv(cnt2.nga, './tbl/centroids/nga_cnt_adm2.csv', row.names = FALSE)

# Ghana -------------------------------------------------------------------

# Download manually from: https://data.humdata.org/dataset/cod-ab-gha
iso <- 'gha'
cnt <- 'Ghana'

## To download
fles.gha <- dir_ls('./shp/gha', regexp = '.shp$')
shp1.gha <- grep('adm1', fles.gha, value = T) %>% st_read()
shp2.gha <- grep('adm2', fles.gha, value = T) %>% st_read() %>% st_make_valid()

## To calculate the centroids / table
cnt1.gha <- coordnts(shp = shp1.gha, lvl = 'ADM1_EN') %>% mutate(iso = 'GHA')
cnt2.gha <- coordnts(shp = shp2.gha, lvl = 'ADM2_EN') %>% mutate(iso = 'GHA')

# To write the files as csv 
write.csv(cnt1.gha, './tbl/centroids/gha_cnt_adm1.csv', row.names = FALSE)
write.csv(cnt2.gha, './tbl/centroids/gha_cnt_adm2.csv', row.names = FALSE)

# Cote d voire ------------------------------------------------------------

# Download manually from: https://data.humdata.org/dataset/cod-ab-civ
iso <- 'civ'
cnt <- 'Cote D Voire'

## To read the downloaded files
fles.civ <- dir_ls('./shp/civ', regexp = '.shp$')
shp1.civ <- grep('adm1', fles.civ, value = T) %>% st_read()
shp2.civ <- grep('adm2', fles.civ, value = T) %>% st_read() %>% st_make_valid()

## To calculate the centroids / table 
cnt1.civ <- coordnts(shp = shp1.civ, lvl = 'ADM1_FR') %>% mutate(iso = 'CIV')
cnt2.civ <- coordnts(shp = shp2.civ, lvl = 'ADM2_FR') %>% mutate(iso = 'CIV')

# To write the files as csv 
write.csv(cnt1.civ, './tbl/centroids/civ_cnt_adm1.csv', row.names = FALSE)
write.csv(cnt2.civ, './tbl/centroids/civ_cnt_adm2.csv', row.names = FALSE)





