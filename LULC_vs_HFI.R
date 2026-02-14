library(terra)
library(dplyr)

# 1. Cargar rasters
LULC <- rast("C:/A_TRABAJO/AA_CHILE/2024_coverage_lclu.tif")
HFI <- rast("C:/A_TRABAJO/AA_CHILE/GIS/HFP_2018_chile.tif")

# 2. Resamplear HFI a la resolución/extensión de LULC
# Usamos 'near' para HFI si son valores continuos no categóricos,
# pero 'bilinear' está bien si prefieres suavizar.
HFI_resampled <- resample(HFI, LULC, method="bilinear")

# 3. Definir diccionario de cobertura (Necesario para el join posterior)
diccionario_cobertura <- data.frame(
  ID = c(1, 3, 59, 60, 67, 10, 11, 12, 63, 66, 29, 14, 9, 18, 15, 22, 24, 23, 61, 25, 26, 33, 34, 27),
  Clase_ES = c("Formación boscosa", "Bosque", "Bosque Primario", "Bosque Secundario", "Bosque Achaparrado", 
               "Formación natural no boscosa", "Humedal", "Pastizal", "Estepa", "Matorral", "Afloramiento Rocoso",
               "Agropecuaria y Silvicultura", "Silvicultura", "Agricultura", "Pastura", 
               "Área sin vegetación", "Infraestructura", "Arena, Playa y Duna", "Salar", "Otra área sin vegetación",
               "Cuerpo de agua", "Río, lago u océano", "Hielo y nieve", "No observado")
)

# 4. Calcular estadística zonal
# OJO: zonal(valores, zonas)
# Queremos el promedio de HFI (valores) agrupado por LULC (zonas)
stats_zonales <- zonal(HFI_resampled, LULC, fun="mean", na.rm=TRUE)
stats_zonales <-relacion_lulc_hfi
# 5. Preparar tabla para el join
df_zonas <- as.data.frame(stats_zonales)
# terra::zonal suele nombrar la columna de resultados con el nombre de la capa de valores
colnames(df_zonas) <- c("ID", "Promedio_HFI")

# 6. Unir con el diccionario de cobertura
df_completo <- left_join(df_zonas, diccionario_cobertura, by = c("ID" = "ID"))

# 7. Calcular estadísticas completas por clase (Promedio, Mediana, Max, Min, SD)
# NOTA: Para obtener todas las estadísticas (min, max, sd), 
# necesitamos trabajar con los valores originales de HFI por zona, 
# no solo con el promedio calculado por zonal().

# Solución: Usar terra::extract para obtener todos los valores y luego agrupar
valores_por_clase <- terra::extract(HFI_resampled, LULC, na.rm=TRUE)
colnames(valores_por_clase) <- c("ID", "HFI_Value")

# Unir valores extraídos con el diccionario
df_valores_clase <- left_join(valores_por_clase, diccionario_cobertura, by = c("ID" = "ID"))

# Calcular estadísticas descriptivas
tabla_resumen <- df_completo %>%
  group_by(Clase_ES) %>%
  summarise(
    Promedio = mean(HFI_Value, na.rm = TRUE),
    Mediana = median(HFI_Value, na.rm = TRUE),
    Maximo = max(HFI_Value, na.rm = TRUE),
    Minimo = min(HFI_Value, na.rm = TRUE),
    Desviacion_Estandar = sd(HFI_Value, na.rm = TRUE),
    Conteo = n()
  ) %>%
  arrange(desc(Promedio))

# 8. Visualizar resultados
print(tabla_resumen)

# 9. Exportar a CSV
write.csv(tabla_resumen, "Resumen_HFI_por_Cobertura.csv", row.names = FALSE)
cat("Tabla exportada exitosamente como 'Resumen_HFI_por_Cobertura.csv'")