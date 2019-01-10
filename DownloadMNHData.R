# clean environment >>
rm(list = ls()[!(ls() %in% c("p_csv","s_csv"))])

setwd("C:/Users/totus tuus/Documents/R_projects/MNH_QoC_Dashboard")
yaml_file <- "MNHQoCDashboardConfig.yml"
yaml_environment <- "default"
config <- config::get(value = NULL, config = yaml_environment, file = Sys.getenv("R_CONFIG_FILE", yaml_file), use_parent = FALSE)

p_url <- config$primarycsvurl
s_url <- config$secondarycsvurl
# comment out for now
p_csv <- read.csv(p_url)
s_csv <- read.csv(s_url)

PMetadata <- read.csv("dashboard/data/prim_metadata_import.csv")
SMetadata <- read.csv("dashboard/SMetadata.csv")

p_dirty_names <- names(p_csv)
s_dirty_names <- names(s_csv)

# pattern <- "^[0-9a-zA-Z_]+(\\.([0-9a-zA-Z_])+){0,10}"
# txt_to_remove <- regmatches(p_dirty_names, regexpr(pattern, p_dirty_names))
pattern <- "([0-9a-zA-Z_]+(\\.){1}){0,10}"
pnames_to_keep_list <- regmatches(p_dirty_names, regexpr(pattern, p_dirty_names), invert = TRUE)
snames_to_keep_list <- regmatches(s_dirty_names, regexpr(pattern, s_dirty_names), invert = TRUE)


pnames_to_keep_vector <- sapply(X = pnames_to_keep_list, FUN = function(x){x[2]})
snames_to_keep_vector <- sapply(X = snames_to_keep_list, FUN = function(x){x[2]})

p_clean_names <- gsub("^(\\.\\.|\\.|_)","",pnames_to_keep_vector)
s_clean_names <- gsub("^(\\.\\.|\\.|_)","",snames_to_keep_vector)

write.csv(x = p_clean_names, "p_csv_names.csv")
write.csv(x = s_clean_names, "s_csv_names.csv")


names(p_csv) <- p_clean_names
names(s_csv) <- s_clean_names


write.csv(x = p_csv, file = "p_csv.csv", row.names = FALSE)
write.csv(x = s_csv, file = "s_csv.csv", row.names = FALSE)





