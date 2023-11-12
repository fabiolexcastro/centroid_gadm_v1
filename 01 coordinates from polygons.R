

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

download.west <- function(iso, cnt){
  
  iso <- 'gha'
  cnt <- 'Ghana'
  
  iso <- 'cmr'
  cnt <- 'Cameroon'
  
  cat('... To download: ', iso, '\t')
  lsts <- search_datasets(cnt, rows = 40)
  dtst <- pluck(lsts, 13)
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

# West Africa -------------------------------------------------------------

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

# Kenya -------------------------------------------------------------------

iso <- 'ken'
cnt <- 'Kenya'

## To download
fles.ken <- download.east(iso = iso, cnt = cnt)
shp1.ken <- grep('adm1', fles.ken, value = T) %>% st_read()
shp2.ken <- grep('adm2', fles.ken, value = T) %>% st_read() %>% st_make_valid()

## To calculate the centroids / table
cnt1.ken <- coordnts(shp = shp1.ken, lvl = 'ADM1_EN') %>% mutate(iso = 'KEN')
cnt2.ken <- coordnts(shp = shp2.ken, lvl = 'ADM2_EN') %>% mutate(iso = 'KEN')

# East Africa -------------------------------------------------------------

# Cameroon ----------------------------------------------------------------

iso <- 'cmr'
cnt <- 'Cameroon'

## To download
fles.cmr <- download(iso = iso, cnt = cnt)
shp1.cmr <- grep('adm1', fles, value = T) %>% st_read()
shp2.cmr <- grep('adm2', fles, value = T) %>% st_read() %>% st_make_valid()

## To calculate the centroids / table
cnt1.cmr <- coordnts(shp = shp1.cmr, lvl = 'ADM1_EN') %>% mutate(iso = 'GHA')
cnt2.cmr <- coordnts(shp = shp2.cmr, lvl = 'ADM2_EN') %>% mutate(iso = 'GHA')

# Nigeria -----------------------------------------------------------------

iso <- 'nga'
cnt <- 'Nigeria'

## To download
fles.nga <- download(iso = iso, cnt = cnt)
shp1.nga <- grep('adm1', fles, value = T) %>% st_read()
shp2.nga <- grep('adm2', fles, value = T) %>% st_read() %>% st_make_valid()

## To calculate the centroids / table
cnt1.cmr <- coordnts(shp = shp1.cmr, lvl = 'ADM1_EN') %>% mutate(iso = 'GHA')
cnt2.cmr <- coordnts(shp = shp2.cmr, lvl = 'ADM2_EN') %>% mutate(iso = 'GHA')

# Ghana -------------------------------------------------------------------

iso <- 'gha'
cnt <- 'Ghana'

## To download
fles.gha <- download(iso = iso, cnt = cnt)
shp1.gha <- grep('adm1', fles.gha, value = T) %>% st_read()
shp2.gha <- grep('adm2', fles.gha, value = T) %>% st_read() %>% st_make_valid()

## To calculate the centroids / table
cnt1.gha <- coordnts(shp = shp1.gha, lvl = 'ADM1_EN') %>% mutate(iso = 'GHA')
cnt2.gha <- coordnts(shp = shp2.gha, lvl = 'ADM2_EN') %>% mutate(iso = 'GHA')

















