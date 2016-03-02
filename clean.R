# NACA HcT & Wellness Community Outreach, Ugbokpo, Apa LGA, Benue State Nigeria
# December 2015
# ----------------------------------------------------------------------------
# HIV Counselling and Testing (rawdat) Data
# ---------------------------------------------------------------------------

# DATA CLEANING
# ==============
library(dplyr)
library(Amelia)

# Load data
rawdat <- read.csv("HCT.txt", na.strings = "", stringsAsFactors = TRUE)
str(rawdat)
tbl_df(rawdat)

# Edit variable names
colnames(rawdat) <- c("id", "state", "lga", "facility", "ref.from", "setting",
                       "name", "age", "sex", "first.time", "date", "code",
                       "residence.state", "residence.lga", "marital.status",
                       "children.U5", "wives", "counsel.type", "prev.negative",
                       "pregnant", "info.routes", "info.risk", "info.methods",
                       "info.result", "informed.consent", "sex.intercourse",
                       "transfusion", "casual.sex", "regular.sex", "sti",
                       "multi.partners", "coughing", "wt.loss", "lymphadeno",
                       "fever", "sweat", "vag.discharge", "lower.abd",
                       "ureth.discharge", "scrotal", "gen.sore", "hiv.result",
                       "form.signed", "insider.form", "rcvd.result",
                       "referred.to", "referred.which", "post.counsel",
                       "risk.reduction", "disclosure.plan", "bring.partner",
                       "bring.children", "fp.info", "contracep.other",
                       "contracep.condom", "condom.correct", "condom.given")
length(colnames(rawdat)) # should be equal to no. of variables
anyNA(colnames(rawdat)) # check if any name was missed
tbl_df(rawdat)

# make adjustments
rawdat$name <- as.character(rawdat$name)
str(rawdat$name)

rawdat$setting[is.na(rawdat$setting)] <- "rawdat"
anyNA(rawdat$setting) # desired output - FALSE

rawdat$marital.status[rawdat$marital.status == "Singe"] <- "Single"
rawdat$marital.status[rawdat$marital.status == "widowed"] <- "Widowed"
rawdat$marital.status <- factor(rawdat$marital.status,
                                 levels = c("Single", "Married", "Divorced", "Widowed"))
head(rawdat$marital.status)

str(rawdat$date)
rawdat$date <- factor(rawdat$date, 
                       levels = c("12/8/2015 0:00:00", "12/9/2015 0:00:00", "12/10/2015 0:00:00"),
                       labels = c("Day 1", "Day 2", "Day 3"),
                       ordered = TRUE)
head(rawdat$date)

# Process missing values
apply(rawdat, 2, function(x) sum(is.na(x))) # missing values per variable
missmap(rawdat,
        main = "Missing values in rawdat dataset") # plot missing values

rawdat$age[is.na(rawdat$age)] <- median(rawdat$age, na.rm = TRUE)
anyNA(rawdat$age)

# Save as file
write.csv(rawdat, file = "hctclean.csv")
