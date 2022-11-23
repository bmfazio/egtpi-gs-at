# pegar las ATs en formato horrible de junio y julio a los UBIGEOS totales
library(dplyr)
library(readxl)

killna <- function(x){y<-x[!is.na(x)];if(length(y)==0){return(NA)}else{return(unique(y))}}

"../egtpi-2022-completo/_bd-manual/EGTPI_BD_19_SETIEMBRE_0900.xlsx" -> where_bd
read_xlsx(where_bd, sheet = "2. DISTRITAL", skip = 1) -> egtpi
names(egtpi)[9] <- "EGTPI"

read_xlsx("in/Matriz de AT_junio y julio 22_final.xlsx",
          sheet = "at") -> at
names(at)[-1] <- c("AT_JUNIO", "MOD_JUNIO", "IAL_JUNIO",
                   "AT_JULIO", "MOD_JULIO", "IAL_JULIO")

egtpi %>%
  select(UBIGEO) %>%
  left_join(
    at %>%
      mutate(AT_JUNIO = as.numeric(!is.na(AT_JUNIO)),
             IAL_JUNIO = as.numeric(!is.na(IAL_JUNIO)),
             AT_JULIO = as.numeric(!is.na(AT_JULIO)),
             IAL_JULIO = as.numeric(!is.na(IAL_JULIO))) %>%
      group_by(UBIGEO) %>%
      summarize(AT_JUNIO = max(AT_JUNIO),
                MOD_JUNIO = killna(MOD_JUNIO),
                IAL_JUNIO = max(IAL_JUNIO),
                AT_JULIO = max(AT_JULIO),
                MOD_JULIO = killna(MOD_JULIO),
                IAL_JULIO = max(IAL_JULIO)),
            by = "UBIGEO") %>%
  writexl::write_xlsx("out/at-vieja.xlsx")