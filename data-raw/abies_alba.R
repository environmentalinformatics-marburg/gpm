abies_alba_01 <- read.table("data-raw/abiesalba_fulldata_01.csv",
                            header = TRUE, sep = ";", dec = ".")
abies_alba_02 <- read.table("data-raw/abiesalba_fulldata_02.csv",
                            header = TRUE, sep = ";", dec = ".")
abies_alba_02$ID <- as.numeric(substr(abies_alba_02$ID, 3, 6))
abies_alba <- merge(abies_alba_01, abies_alba_02, by = "ID")
devtools::use_data(abies_alba, overwrite = TRUE)
