daterange_vector <- c("082022", "092022",
                      "102022", "112022") # for regional AT
# ---- first block ----

source("R/1_setup.R", encoding = "utf-8")
source("R/2_load.R", encoding = "utf-8")

forms.at %>%
  arrange(`Marca temporal`) %>%
  transmute(
    N_ROW = 1:n(),
    DEPARTAMENTO = simpletext(Departamento),
    PROVINCIA = simpletext(Provincia),
    DISTRITO = simpletext(Distrito),
    FECHA = as_date(`Fecha de la Asistencia Técnica`),
    RESPONSABLE = simpletext(`Unidad Orgánica o Programa Social del responsable que brinda la asistencia técnica`),
    MODALIDAD = `Modalidad que brindó la asistencia técnica`,
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
    RESPONSABLE =
      case_when(
        CARGO %in% c(
          "COORDINADOR DE ENLACE", "CORDINADOR ENLACE MIDIS"
          ) ~ "DIRECCION DE ARTICULACION TERRITORIAL",
        TRUE ~ RESPONSABLE
      ),
    CATEGORIA =
      case_when(
          RECIBE == "Equipo Territorial MIDIS (Regional)" &
            RESPONSABLE == "DIRECCION DE ARTICULACION TERRITORIAL" ~ "AT_ETR",
          RECIBE == "Equipo Territorial MIDIS (Local)" &
            RESPONSABLE == "DIRECCION DE ARTICULACION TERRITORIAL" ~ "AT_ETL",
          RECIBE %in% c("Gobierno Regional", "IAR") &
            RESPONSABLE == "DIRECCION DE ARTICULACION TERRITORIAL" ~ "AT_GORE",
          RECIBE %in% c("Gobierno Local", "IAL") &
            RESPONSABLE == "DIRECCION DE ARTICULACION TERRITORIAL" ~ "AT_GL",
          RECIBE %in% c("Gobierno Local", "IAL") &
            RESPONSABLE %in%
            c("CUNA MAS", "JUNTOS", "PAIS", "PENSION 65", "QALI WARMA") ~ "AT_PPSS")
  ) %>% select(-CARGO) -> at.pretidy

###
at.pretidy %>% write_xlsx("mid/tabla-at.xlsx")
at.pretidy %>% filter(is.na(CATEGORIA)) %>% write_xlsx("mid/exclusiones-categoria.xlsx")
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
      filter(FECHA >= dmy(paste0("01",daterange_vector[1])),
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

# ---- second block ----
#### INTERVENCION MANUAL PARA CORREGIR EMPALMES

read_xlsx("_FUZZY-at-local-LISTO.xlsx") -> fuzz_listo

at.tidy %>%
  left_join(fuzz_listo %>% select(N_ROW, RESULTADO), by = "N_ROW") %>%
  filter(RESULTADO == "NO") %>%
  select(-RESULTADO) %>%
  write_xlsx("mid/exclusiones-distrito.xlsx")

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
                name = CATEGORIA,
                fecha = paste0(sprintf("%02d", month(FECHA)), year(FECHA)),
                type = ifelse(RESPONSABLE == "DIRECCION DE ARTICULACION TERRITORIAL",
                              "CE", "PPSS"),
                modal = MODALIDAD,
                value = RESPONSABLE),
    by = "N_ROW"
  ) %>%
  group_by(UBIGEO, fecha) %>%
  mutate(modal = paste0(sort(unique(modal)), collapse = " y ")) %>%
  group_by(UBIGEO, name, type, fecha) %>%
  filter(N_ROW == min(N_ROW)) %>%
  ungroup %>%
  select(-N_ROW, -type) -> at_local

#
egtpi %>%
  select(UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO) %>%
  crossing(tibble(fecha = c(daterange_vector))) %>%
  crossing(tibble(name = c("AT_GL", "AT_PPSS", "CARGO_PPSS", "MODALIDAD"))) %>%
  left_join(
    bind_rows(
      at_local %>% mutate(value = "1"),
    at_local %>%
      filter(value != "DIRECCION DE ARTICULACION TERRITORIAL") %>%
      mutate(name = "CARGO_PPSS"),
    at_local %>%
      mutate(name = "MODALIDAD",
             value = modal)) %>%
      select(-modal) %>%
      unique,
    by = c("UBIGEO", "fecha", "name")) %>%
  mutate(
    value = case_when(
      !(name %in% c("CARGO_PPSS", "MODALIDAD")) & is.na(value) ~ "0",
      TRUE ~ value
    ),
    name = paste0(name, "_", fecha)) %>%
  select(-fecha) %>%
  pivot_wider() %>%
  arrange(UBIGEO) %>%
  write_xlsx("at-local.xlsx")