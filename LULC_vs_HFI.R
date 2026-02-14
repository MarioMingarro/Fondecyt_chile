library(terra)
library(dplyr)

LULC <- rast("C:/A_TRABAJO/AA_CHILE/2024_coverage_lclu.tif")
HFI <- rast("C:/A_TRABAJO/AA_CHILE/GIS/HFP_2018_chile.tif")

# 2. Diccionario
diccionario_cobertura <- data.frame(
  ID = c(1, 3, 59, 60, 67, 10, 11, 12, 63, 66, 29, 14, 9, 18, 15, 22, 24, 23, 61, 25, 26, 33, 34, 27),
  Clase_ES = c("Formación boscosa", "Bosque", "Bosque Primario", "Bosque Secundario", "Bosque Achaparrado", 
               "Formación natural no boscosa", "Humedal", "Pastizal", "Estepa", "Matorral", "Afloramiento Rocoso",
               "Agropecuaria y Silvicultura", "Silvicultura", "Agricultura", "Pastura", 
               "Área sin vegetación", "Infraestructura", "Arena, Playa y Duna", "Salar", "Otra área sin vegetación",
               "Cuerpo de agua", "Río, lago u océano", "Hielo y nieve", "No observado")
)


if (!compareGeom(LULC, HFI, stopOnError = FALSE)) {
  message("Los rasters no están alineados. Reproyectando/Resampleando HFI...")
  HFI <- resample(HFI, LULC, method = "bilinear")
}


medias_zonal <- zonal(HFI, LULC, fun = "mean", na.rm = TRUE)
colnames(medias_zonal) <- c("ID","HFI_mean")

# sd_zonal <- zonal(HFI, LULC, fun = "sd", na.rm = TRUE)
# colnames(sd_zonal) <- c("ID","HFI_sd")

count_zonal <- freq(LULC)
count_zonal <- count_zonal[,-1]
colnames(count_zonal) <- c("ID","n")


resumen_final <- medias_zonal %>%
  left_join(diccionario_cobertura, by = "ID")  %>%
  left_join(count_zonal, by = "ID")


resultados <-  na.omit(resumen_final)
print(resumen_final)
write.csv2(resultados, "C:/A_TRABAJO/AA_CHILE/LULC_HFI.csv")
