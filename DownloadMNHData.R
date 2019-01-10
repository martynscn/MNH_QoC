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

p_csv_import <- read.csv(file = "p_csv.csv", row.names = NULL, as.is = TRUE)
s_csv_import <- read.csv(file = "s_csv.csv", row.names = NULL, as.is = TRUE)


sec_metadata_import <- read.csv(file = "dashboard/data/indDefndf_Sec.csv", as.is = TRUE)
prim_metadata_import <- read.csv(file = "dashboard/data/prim_metadata_import.csv", as.is = TRUE)

OrgUnitsExport <- read.csv("dashboard/data/MNH_OrgUnitsExport.csv", stringsAsFactors = TRUE)

primDataTemp1 <- left_join(x = p_csv_import, y = OrgUnitsExport[,c("FacilityName","FacilityLabel")], by = c("name_of_facility" = "FacilityName"))
primDataTemp2 <- left_join(x = primDataTemp1, y = OrgUnitsExport[,c("StateName","StateLabel")], by = c("state" = "StateName"))
primDataTemp3 <- left_join(x = primDataTemp2, y = OrgUnitsExport[,c("LGAName","LGALabel")], by = c("local_government_area_lga" = "LGAName"))
primDataTemp4 <- left_join(x = primDataTemp3, y = OrgUnitsExport[,c("SenatorialDistName","SenatorialDistLabel")], by = c("senatorial_district" = "SenatorialDistName"))

# primData_org_units_Names <- names(primDataTemp4)[(ncol(primDataTemp4) - 3):ncol(primDataTemp4)]
# prim_org_units_metadata <- data.frame(Col_Name = primData_org_units_Names, Data_Type = "Factor", Computed.question.name = c("Facility Label", "State Label","LGA Label", "Senatorial District Label"))
# prim_metadata <- rbind(prim_metadata_import, prim_org_units_metadata)
# prim_metadata <- prim_metadata_import

primDataTemp5_List <- sapply(X = 1:ncol(primDataTemp4),
                        FUN = function(j) {
                          x <- primDataTemp4[,j]
                          y <- names(primDataTemp4)[j]
                          column_class <- prim_metadata_import[prim_metadata_import$Col_Name == y,2][1]
                          if(column_class == "character") {
                            as.character(x)
                          } else if(column_class == "logical") {
                            as.logical.factor(x)
                          } else if(column_class == "factor") {
                            as.factor(x)
                          } else if(column_class == "numeric") {
                            as.numeric(x)
                          } else if(column_class == "integer") {
                            as.integer(x)
                          } else if(column_class == "Date-time") {
                            ymd_hms(x, quiet = TRUE)
                          } else if(column_class == "Image") {
                            as.character(x)
                          } else if(column_class == "geo-coordinates") {
                            as.character(x)
                          } else if(column_class == "Time") {
                            hm(x, quiet = TRUE)
                          } else if(column_class == "Date") {
                            ymd(x)
                          } else if(column_class == "CharNum") {
                            as.numeric(as.character(x))
                          }
                        }, simplify = FALSE, USE.NAMES = TRUE)

# ll <- sapply(primDataTemp5_List,length)
# cols_that_didnot_convert <- names(primDataTemp4)[ll == 0]
primDataTemp5_df <- as.data.frame(primDataTemp5_List, row.names = NULL,stringsAsFactors = FALSE, col.names = names(primDataTemp4))

primData <- primDataTemp5_df
prim_Data_Types <- sapply(primData, class, simplify = TRUE, USE.NAMES = FALSE)
prim_numeric_cols <- prim_metadata_import[prim_Data_Types == "numeric"|prim_Data_Types == "integer",]
prim_factor_cols <- prim_metadata_import[prim_Data_Types == "factor",]
prim_logical_cols <- prim_metadata_import[prim_Data_Types == "logical",]
prim_character_cols <- prim_metadata_import[prim_Data_Types == "character",]
prim_date_cols <- prim_metadata_import[prim_Data_Types == "Date",]
prim_period_cols <- prim_metadata_import[prim_Data_Types == "Period",]
prim_posixct_cols <- prim_metadata_import[prim_Data_Types == "POSIXct",]



#You can add for date, date-time and time

# logical,character,Date,numeric,integer,factor



