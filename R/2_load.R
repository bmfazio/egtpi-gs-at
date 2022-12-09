gs4_deauth()
read_sheet("1OxxL8CCqv5xSrqtadOgsWamitTncAvAnyIZekXa0G5w") -> forms.at

"../egtpi-2022-completo/_bd-manual/EGTPI_BD_19_SETIEMBRE_0900.xlsx" -> where_bd
read_xlsx(where_bd, sheet = "2. DISTRITAL", skip = 1) -> egtpi
names(egtpi)[9] <- "EGTPI"

read_xlsx("in/IAL CUNAMAS.xlsx") -> at_cunamas
read_xlsx("out/at-vieja.xlsx") -> at_vieja