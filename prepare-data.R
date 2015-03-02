library(foreign)
library(lubridate)

decimal_date_na <- function(x){ y <- rep(NA, length(x));  y[!is.na(x)] <- decimal_date(x[!is.na(x)]); return(y) }

setwd("~/Documents/Research/ALPHA/prepare-data/")


##############################
####  Residence episodes  ####
##############################

aginc.res <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec1_Agincourt_clean_vStata12.dta")
ifakara.res <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec1_Ifakara_clean_vStata12.dta")
karonga.res <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec1_Karonga_clean_vStata12.dta")
kisesa.res <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec1_Kisesa_clean_vStata12.dta")
kisumu.res <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec1_Kisumu_clean_vStata12.dta")
manic.res <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec1_Manicaland_clean_vStata12.dta")
masaka.res <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec1_Masaka_clean_vStata12.dta")
nairobi.res <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec1_Nairobi_clean_vStata12.dta") 
rakai.res <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec1_Rakai_clean_vStata12.dta")
umkhan.res <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec1_uMkhanyakude_clean_vStata12.dta")

table(aginc.res$sex)
table(ifakara.res$sex)
table(karonga.res$sex)
table(kisesa.res$sex)
table(kisumu.res$sex)
table(manic.res$sex)
table(masaka.res$sex)
table(nairobi.res$sex)
table(rakai.res$sex)
table(umkhan.res$sex)

aginc.res$sex <- factor(aginc.res$sex, 1:2, c("Male", "Female"))
kisesa.res$sex <- factor(kisesa.res$sex, 1:2, c("Male", "Female"))
masaka.res$sex <- factor(as.integer(masaka.res$sex), 1:2, c("Male", "Female"))
nairobi.res$sex <- factor(as.integer(nairobi.res$sex), 1:2, c("Male", "Female"))
rakai.res$sex <- factor(rakai.res$sex, 1:2, c("Male", "Female"))
umkhan.res$sex <- factor(as.integer(umkhan.res$sex), 1:2, c("Male", "Female"))


class(aginc.res$dob)
class(ifakara.res$dob)
class(karonga.res$dob)
class(kisesa.res$dob)
class(kisumu.res$dob)
class(manic.res$dob)
class(masaka.res$dob)
class(nairobi.res$dob)
class(rakai.res$dob)
class(umkhan.res$dob)

class(aginc.res$exit_date)
class(ifakara.res$exit_date)
class(karonga.res$exit_date)
class(kisesa.res$exit_date)
class(kisumu.res$exit_date)
class(manic.res$exit_date)
class(masaka.res$exit_date)
class(nairobi.res$exit_date)
class(rakai.res$exit_date)
class(umkhan.res$exit_date)

kisesa.res$dob <- as.Date(kisesa.res$dob, origin="1960-01-01")
manic.res$dob <- as.Date(manic.res$dob, origin="1960-01-01")

manic.res$entry_date <- as.Date(manic.res$entry_date, origin="1960-01-01")
rakai.res$entry_date <- as.Date(rakai.res$entry_date, origin="1960-01-01")

manic.res$exit_date <- as.Date(manic.res$exit_date, origin="1960-01-01")
rakai.res$exit_date <- as.Date(rakai.res$exit_date, origin="1960-01-01")

exittype.levels <- c("present in study site", "death", "out-migration", "lost to follow-up")

aginc.res$exit_type <- factor(aginc.res$exit_type, 1:4, exittype.levels)
ifakara.res$exit_type <- factor(as.integer(ifakara.res$exit_type), 1:4, exittype.levels)
karonga.res$exit_type <- factor(as.integer(karonga.res$exit_type), 1:4, exittype.levels)
kisesa.res$exit_type <- factor(as.integer(kisesa.res$exit_type), 1:4, exittype.levels)
kisumu.res$exit_type <- factor(as.integer(kisumu.res$exit_type), 1:4, exittype.levels)
manic.res$exit_type <- factor(as.integer(manic.res$exit_type), 1:4, exittype.levels)
masaka.res$exit_type <- factor(as.integer(masaka.res$exit_type), 1:4, exittype.levels)
nairobi.res$exit_type <- factor(as.integer(nairobi.res$exit_type), 1:4, exittype.levels)
rakai.res$exit_type <- factor(as.integer(rakai.res$exit_type), 1:4, exittype.levels)
umkhan.res$exit_type <- factor(as.integer(umkhan.res$exit_type), 1:4, exittype.levels)

res.cols <- c("idno", "sex", "dob", "entry_date", "exit_date", "exit_type", "study_name")
res <- rbind(aginc.res[,res.cols], ifakara.res[,res.cols], karonga.res[,res.cols],
             kisesa.res[,res.cols], kisumu.res[,res.cols], manic.res[,res.cols],
             masaka.res[,res.cols], nairobi.res[,res.cols], rakai.res[,res.cols],
             umkhan.res[,res.cols])

table(res$sex, res$study_name)
table(res$exit_type, res$study_name)


#########################
####  HIV test data  ####
#########################

aginc.hiv <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec2b_Agincourt_clean_vStata12.dta")
ifakara.hiv <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec2b_Ifakara_clean_vStata12.dta")
karonga.hiv <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec2b_Karonga_clean_vStata12.dta")
kisesa.hiv <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec2b_Kisesa_clean_vStata12.dta")
kisumu.hiv <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec2b_Kisumu_clean_vStata12.dta")
manic.hiv <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec2b_Manicaland_clean_vStata12.dta")
masaka.hiv <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec2b_Masaka_clean_vStata12.dta")
nairobi.hiv <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec2b_Nairobi_clean_vStata12.dta")
rakai.hiv <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec2b_Rakai_clean_vStata12.dta")
umkhan.hiv <- read.dta("~/Documents/Data/ALPHA/Gates/v2015-02-21/ALPHA6_spec2b_uMkhanyakude_clean_vStata12.dta")

kisesa.hiv$hiv_test_date <- as.Date(kisesa.hiv$hiv_test_date, origin="1960-01-01")
kisesa.hiv$test_report_date <- as.Date(kisesa.hiv$test_report_date, origin="1960-01-01")

manic.hiv$hiv_test_date <- as.Date(manic.hiv$hiv_test_date, origin="1960-01-01")
manic.hiv$test_report_date <- as.Date(manic.hiv$test_report_date, origin="1960-01-01")

masaka.hiv$hiv_test_date <- as.Date(masaka.hiv$hiv_test_date, origin="1960-01-01")
masaka.hiv$test_report_date <- as.Date(masaka.hiv$test_report_date, origin="1960-01-01")

rakai.hiv$hiv_test_date <- as.Date(rakai.hiv$hiv_test_date, origin="1960-01-01")
rakai.hiv$test_report_date <- as.Date(rakai.hiv$test_report_date, origin="1960-01-01")


source.levels <- c("part of a population based study",
                   "part of a special research study",
                   "clinical record",
                   "self-reported by respondent",
                   "va report proxy respondent")

hivresult.levels <- c("negative", "positive", "indeterminate", "not reported")
                   

aginc.hiv$source_of_test_information <- factor(aginc.hiv$source_of_test_information, 1:5, source.levels)
kisesa.hiv$source_of_test_information <- factor(kisesa.hiv$source_of_test_information, 1:5, source.levels)
karonga.hiv$source_of_test_information <- factor(as.integer(karonga.hiv$source_of_test_information), 1:5, source.levels)
manic.hiv$source_of_test_information <- factor(as.integer(manic.hiv$source_of_test_information), 1:5, source.levels)
masaka.hiv$source_of_test_information <- factor(masaka.hiv$source_of_test_information, 1:5, source.levels)
nairobi.hiv$source_of_test_information <- factor(as.integer(nairobi.hiv$source_of_test_information), 1:5, source.levels)
rakai.hiv$source_of_test_information <- factor(rakai.hiv$source_of_test_information, 1:5, source.levels)
umkhan.hiv$source_of_test_information <- factor(as.integer(umkhan.hiv$source_of_test_information), 1:5, source.levels)


aginc.hiv$hiv_test_result <- factor(aginc.hiv$hiv_test_result, 0:3, hivresult.levels)
karonga.hiv$hiv_test_result <- factor(karonga.hiv$hiv_test_result, 0:3, hivresult.levels)
kisesa.hiv$hiv_test_result <- factor(kisesa.hiv$hiv_test_result, c(0, 1, 9, 3), hivresult.levels)
manic.hiv$hiv_test_result <- factor(as.integer(manic.hiv$hiv_test_result), 1:4, hivresult.levels)
nairobi.hiv$hiv_test_result <- factor(as.integer(nairobi.hiv$hiv_test_result), 1:4, hivresult.levels)
rakai.hiv$hiv_test_result <- factor(as.integer(rakai.hiv$hiv_test_result), 1:4, hivresult.levels)

hiv <- rbind(aginc.hiv, ifakara.hiv, karonga.hiv, kisesa.hiv, kisumu.hiv,             
             manic.hiv, masaka.hiv, nairobi.hiv, rakai.hiv, umkhan.hiv)




#####################################
####  Create individual dataset  ####
#####################################


ind <- res[,c("idno_orig", "study_name", "sex", "dob")]
ind <- ind[!duplicated(ind),]

ind$dob <- as.Date(as.integer(ind$dob), origin="1970-01-01")
ind$dob <- decimal_date_na(ind$dob)

## Check if dob and sex are unique for all individuals
tmp <- duplicated(ind[,c("idno", "site")])
subset(ind, idno %in% tmp$idno)
ind <- subset(ind, !tmp)
rm(tmp)

## Assign new unique idno
ind$id <- 1:nrow(ind)



#####################################
####  Create residences dataset  ####
#####################################

res$site <- res$study_name

res <- res[,c("idno", "site", "entry_date", "exit_date", "exit_type")]
res$res_id <- 1:nrow(res)
res$entry_date <- as.Date(as.integer(res$entry_date), origin="1970-01-01")
res$exit_date <- as.Date(as.integer(res$exit_date), origin="1970-01-01")

res$entry <- decimal_date_na(res$entry_date)
res$exit <- decimal_date_na(res$exit_date)
res$death <- res$exit_type=="death"

res <- merge(ind, res)
res <- res[,c("res_id", "id", "idno", "site", "sex", "dob", "entry", "exit", "death")]


## ## Merge any contiguous residence episodes

## res.merge <- merge(res, res, 
##                    by.x=c("id", "exit_date"),
##                    by.y=c("id", "entry_date"), suffixes=c("1", "2"))
## res.merge <- subset(res.merge, res_id1 != res_id2)

## resm <- res.merge[,c("res_id1", "res_id2")]

## tmp <- merge(resm, resm, by.x="res_id2", by.y="res_id1")

## table(res.merge$exit_type1)
## table(res.merge$exit_type2)


## res.new <- res
## res.new <- subset(res.new, ! (res_id %in% res.merge$res_id2))

## res.new <- merge(res.new,
##                  setNames(res.merge[,c("res_id1", "exit_date.1", "exit_type2")], c("res_id", "exit_date.upd", "exit_type.upd")),
##                  all.x=TRUE)

## res.new$exit_date[!is.na(res.new$exit_date.upd)], res.new$exit_date.upd[!is.na(res.new$exit_date)]



##############################
####  Create HIV dataset  ####
##############################

hiv$site <- hiv$study_name
hiv$testdate <- decimal_date_na(hiv$hiv_test_date)

hiv$result <- hiv$hiv_test_result
hiv$result[!hiv$result %in% c("negative", "positive")] <- NA
hiv$result <- hiv$result == "positive"

hiv$type <- factor(as.integer(hiv$source_of_test_information), 1:5, c("survey", "special study", "clinic", "self report", "VA"))

hiv <- hiv[,c("idno", "site", "testdate", "result", "type")]
hiv <- merge(ind, hiv, all.y=TRUE)

with(subset(hiv, is.na(id)), table(site))
with(subset(hiv, is.na(id)), table(site, type))

save(ind, res, hiv, file="ALPHA-data_v2015-02-23.RData")

save(ind, file="ALPHA-ind-data_v2015-02-23.RData")
save(res, file="ALPHA-res-data_v2015-02-23.RData")
save(hiv, file="ALPHA-hiv-data_v2015-02-23.RData")
