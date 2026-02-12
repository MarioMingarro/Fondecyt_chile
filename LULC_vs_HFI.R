library(terra)

LULC <- rast("C:/A_TRABAJO/AA_CHILE/2024_coverage_lclu.tif")
HFI <- rast("C:/A_TRABAJO/AA_CHILE/hfp2018.tif")
crs(LULC)
crs(HFI)
LULC <- terra::project(LULC, crs(HFI))
HFI <- terra::mask(HFI, terra::crop(LULC))