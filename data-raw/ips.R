ips <- read.table("data-raw/SNPs_TOP_Borkenkaefer.csv",
                  header = TRUE, sep = ";", dec = ".")


ips_clean <- read.table("data-raw/input_clean_impute.csv",
                  header = TRUE, sep = ";", dec = ".")

devtools::use_data(ips, overwrite = TRUE)
devtools::use_data(ips_clean, overwrite = TRUE)

