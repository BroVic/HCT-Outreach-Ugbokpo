# NACA HcT & Wellness Community Outreach, Ugbokpo, Apa LGA, Benue State Nigeria
# December 2015
# ----------------------------------------------------------------------------
# HIV Counselling and Testing (rawdat) Data
# ---------------------------------------------------------------------------

# DATA CLEANING
# ==============
library(Amelia)

# Load data
rawdat <- read.csv("HCT.txt", na.strings = "", stringsAsFactors = TRUE)
str(rawdat)
dplyr::tbl_df(rawdat)

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
dplyr::tbl_df(rawdat)

# make adjustments
rawdat$name <- as.character(rawdat$name)
str(rawdat$name)

rawdat$setting[is.na(rawdat$setting)] <- "HCT"
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

# replace missing age entries with median
rawdat$age[is.na(rawdat$age)] <- median(rawdat$age, na.rm = TRUE)
anyNA(rawdat$age)

# subset by removing redundant variables
rawdat <- rawdat[, -c(43:57)]
subset(rawdat, select = c(counsel.type, wives, children.U5, lower.abd,
                          vag.discharge, scrotal, gen.sore, pregnant))
complete.cases(rawdat)

# split into male and female groups
grouped <- split(rawdat, rawdat$sex)
missmap(grouped$Female, main = "Missingness Map for Females")
missmap(grouped$Male, main = "Missingness Map for Males")



# Save as file
write.csv(rawdat, file = "hctclean.csv")
