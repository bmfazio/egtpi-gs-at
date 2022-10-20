source("R/1_setup.R", encoding = "utf-8")
source("R/2_load.R", encoding = "utf-8")

daterange_vector <- c("092022", "102022") # for regional AT

forms.at %>%
  transmute(
    N_ROW = 1:n(),
    DEPARTAMENTO = simpletext(Departamento),
    PROVINCIA = simpletext(Provincia),
    DISTRITO = simpletext(Distrito),
    FECHA = as_date(`Fecha de la Asistencia Técnica`),
    CARGO = simpletext(`Cargo del responsable`),
    INTERVENCION = `Intervención en la que se brindó asistencia técnica`,
    RECIBE = `Institución que recibió la asistencia técnica`
  ) %>%
  filter(N_ROW != 4,
         !is.na(DEPARTAMENTO),
         grepl("EGTPI|Sello Municipal", INTERVENCION)
         ) %>%
  mutate(
    DEPARTAMENTO =
      case_when(DEPARTAMENTO == "LIMA METROPOLITANA" ~ "LIMA M",
                TRUE ~ DEPARTAMENTO),
    CATEGORIA =
      case_when(
          RECIBE == "Equipo Territorial MIDIS (Regional)" &
          (grepl("CE", CARGO)|
          CARGO == "COORDINADOR TERRITORIAL") ~ "AT_ETR",
          RECIBE == "Equipo Territorial MIDIS (Local)" &
          (grepl("CE", CARGO)|
          CARGO == "COORDINADOR TERRITORIAL") ~ "AT_ETL",
          RECIBE %in% c("Gobierno Regional", "IAR") &
          (grepl("CE", CARGO)|
          CARGO == "COORDINADOR TERRITORIAL") ~ "AT_GORE",
          RECIBE %in% c("Gobierno Local", "IAL") &
          (grepl("CE", CARGO)|
          CARGO == "COORDINADOR TERRITORIAL") ~ "AT_GL",
          RECIBE %in% c("Gobierno Local", "IAL") &
            grepl("GESTO|GRSTO|ACOMP|ACONP|MONIT|ASIST|CTZ|ZONAL|^AT$", toupper(CARGO)) ~ "AT_PPSS")
  ) -> at.pretidy

###
at.pretidy %>% filter(is.na(CATEGORIA)) %>% write_xlsx("mid/exclusiones-at.xlsx")
at.pretidy %>% filter(!is.na(CATEGORIA)) -> at.tidy
if(any(!unique(at.tidy$DEPARTAMENTO) %in% unique(egtpi$DEPARTAMENTO)))stop("DEPARTAMENTO NO IDENTIFICADO")

egtpi %>%
  pull(DEPARTAMENTO) %>%
  unique %>%
  rep(each = 3*length(daterange_vector)) %>%
  tibble(DEPARTAMENTO = .,
         name = rep(paste0(c("AT_ETR", "AT_ETL", "AT_GORE"), "_",
                           rep(daterange_vector, each = 3)),
                    times = length(.)/(3*length(daterange_vector)))) %>%
  left_join(
    at.tidy %>%
      filter(FECHA >= "2022-09-01",
             CATEGORIA %in% c("AT_ETR", "AT_ETL", "AT_GORE")) %>%
      transmute(
        DEPARTAMENTO,
        name = paste0(CATEGORIA,"_",
                      sprintf("%02d",month(FECHA)),
                      year(FECHA))) %>%
      group_by(DEPARTAMENTO, name) %>%
      summarise(value = n()),
    by = c("DEPARTAMENTO","name")) %>%
  mutate(value = ifelse(is.na(value),0,value)) %>%
  pivot_wider -> at_regional

write_xlsx(at_regional, "at-regional.xlsx")

at.tidy %>%
  filter(FECHA >= "2022-08-01",
         !is.na(DISTRITO),
         DISTRITO != "NA",
         CATEGORIA %in% c("AT_GL", "AT_PPSS")) %>%
  select(DEPARTAMENTO, PROVINCIA, DISTRITO, N_ROW) %>%
  fuzzy_ubigeo(
    egtpi %>% select(UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO)
  ) %>%
  select(-best_match) %>%
  arrange(N_ROW) -> fuzz_prefix

write_xlsx(fuzz_prefix, "_FUZZY-at-local.xlsx")

#### INTERVENCION MANUAL PARA CORREGIR EMPALMES

read_xlsx("_FUZZY-at-local-LISTO.xlsx") -> fuzz_listo

fuzz_listo %>%
  filter(RESULTADO != "NO") %>%
  transmute(
    UBIGEO =
      case_when(RESULTADO == "OK" ~ UBIGEO,
                TRUE ~ RESULTADO),
    N_ROW) %>%
  unique %>%
  left_join(
    at.tidy %>%
      filter(FECHA >= "2022-08-01") %>%
      transmute(N_ROW,
                name = paste0(CATEGORIA, "_", month(FECHA)),
                value = 1),
    by = "N_ROW"
  ) %>% ### REVISAR Q ONDA CON N_ROW == 156, POR AHORA LO IGNORAMOS
  filter(N_ROW != 156) %>%
  select(-N_ROW) %>%
  unique %>%
  pivot_wider(values_fill = 0) %>%
  left_join(egtpi %>% select(UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO),
            .,
            by = "UBIGEO") %>%
  replace_na(list(AT_GL_8 = 0,
                  AT_PPSS_8 = 0,
                  AT_GL_9 = 0,
                  AT_PPSS_9 = 0,
                  AT_GL_10 = 0,
                  AT_PPSS_10 = 0)) %>%
  arrange(UBIGEO) %>%
  write_xlsx("at-local.xlsx")