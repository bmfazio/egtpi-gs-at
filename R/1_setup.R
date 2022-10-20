library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(fuzzyjoin)
library(lubridate)
library(googlesheets4)

###
fuzzy_ubigeo <- function(x, y){
  if(nrow(x) == 0)return(NULL)
  y %>%
    mutate(DEPARTAMENTO =
             ifelse(substr(UBIGEO, 1, 2) == "15",
                    "LIMA", DEPARTAMENTO)) %>%
    select(UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO) %>%
    stringdist_join(x,
                    by = c("DEPARTAMENTO", "PROVINCIA", "DISTRITO"),
                    mode = "left",
                    ignore_case = TRUE, 
                    method = "jw", 
                    max_dist = 1, 
                    distance_col = "dist") %>%
    group_by(DEPARTAMENTO.y, PROVINCIA.y, DISTRITO.y) %>%
    mutate(best_match = min(DEPARTAMENTO.dist)) %>%
    filter(best_match > 0 | DEPARTAMENTO.dist == best_match) %>%
    slice_min(order_by = DEPARTAMENTO.dist, n = 3) %>% 
    mutate(best_match = min(PROVINCIA.dist)) %>%
    filter(best_match > 0 | PROVINCIA.dist == best_match) %>%
    slice_min(order_by = PROVINCIA.dist, n = 2) %>%
    slice_min(order_by = (PROVINCIA.dist+1)*(DISTRITO.dist+1)+DISTRITO.dist, n = 1) %>%
    mutate(dist = DEPARTAMENTO.dist + PROVINCIA.dist + DISTRITO.dist) %>%
    relocate(DISTRITO.dist, .after = PROVINCIA.dist)
}

simpletext <- function(x)toupper(iconv(x,from="UTF-8",to="ASCII//TRANSLIT"))