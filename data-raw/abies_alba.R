abies_alba_01 <- read.table("data-raw/abiesalba_fulldata_01.csv",
                           header = TRUE, sep = ";", dec = ".")
abies_alba_02 <- read.table("data-raw/abiesalba_fulldata_02.csv",
                            header = TRUE, sep = ";", dec = ".")
abies_alba_phenotypes <- read.table("data-raw/abiesalba_phenotypes.csv",
                            header = TRUE, sep = ",", dec = ".")

abies_alba_02$ID <- as.numeric(substr(abies_alba_02$ID, 3, 6))

abies_alba_budburst <- merge(abies_alba_01[, 1:8], abies_alba_02, by = "ID")

abies_abla_misc <-  merge(abies_alba_01[,1:8], abies_alba_phenotypes, by = "ID")
abies_abla_misc <- merge(abies_abla_misc, abies_alba_02, by = "ID")

devtools::use_data(abies_alba_budburst, overwrite = TRUE)
devtools::use_data(abies_abla_misc, overwrite = TRUE)


