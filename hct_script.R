## HIV Counselling and Testing (HCT) Data

# Load data
hctdata <- read.csv("HCT.txt", na.strings = "", stringsAsFactors = TRUE)
str(hct)

# Edit variable names
colnames(hct) <- c("id", "state", "lga", "facility", "ref.from", "setting", "name", "age", "sex", "first.time", "date", "code", "residence.state", "residence.lga", "marital.status", "children.U5", "wives", "counsel.type", "prev.negative", "pregnant", "info.routes", "info.risk", "info.methods", "info.result", "informed.consent", "sex.intercourse", "transfusion", "casual.sex", "regular.sex", "sti", "multi.partners", "coughing", "wt.loss", "lymphadeno", "fever", "sweat", "vag.discharge", "lower.abd", "ureth.discharge", "scrotal", "gen.sore", "hiv.result", "form.signed", "insider.form", "rcvd.result", "referred.to", "referred.which", "post.counsel", "risk.reduction", "disclosure.plan", "bring.partner", "bring.children", "fp.info", "contracep.other", "contracep.condom", "condom.correct", "condom.given")
length(colnames(hct)) # should be equal to no. of variables
anyNA(colnames(hct)) # check if any name was missed
str(hct)

# make adjustments
hct$name <- as.character(hct$name)
str(hct$name)

hct$setting[is.na(hct$setting)] <- "HCT"
anyNA(hct$setting) # desired output - FALSE

hct$marital.status[hct$marital.status == "Singe"] <- "Single"
hct$marital.status[hct$marital.status == "widowed"] <- "Widowed"
hct$marital.status <- factor(hct$marital.status,
                             levels = c("Single", "Married", "Divorced", "Widowed"))
head(hct$marital.status)

str(hct$date)
hct$date <- factor(hct$date, 
                   levels = c("12/8/2015 0:00:00", "12/9/2015 0:00:00", "12/10/2015 0:00:00"),
                   labels = c("Day 1", "Day 2", "Day 3"),
                   ordered = TRUE)
head(hct$date)

# Process missing values
apply(hct, 2, function(x) sum(is.na(x))) #quantifying missing values per variable
Amelia::missmap(hct,
                main = "Missing values in HCT dataset") # plotting missing values

hct$age[is.na(hct$age)] <- median(hct$age, na.rm = TRUE)
anyNA(hct$age)

# ExPLORATORY ANALYSIS
# Functions to be used
custom_plot <- function(){ # adjusts y-axis and adds horizontal gridlines
  axis(2, las = 2)
  grid(NA, ny = NULL, col = "lightgrey", lty = "dashed")
}

percent <- function(x) prop.table(table(x))*100 #yields proportion table in percent value


# Generate an overview/summary of the data
print(summary(hct))

# No. of persons counselled and tested
days <- table(hct$date)
days
barplot(days,
        main = "No. of persons counselled and tested",
        ylim = c(0, 120),
        col = "blue",
        ylab = "No. of persons",
        axes = FALSE)
custom_plot()

# Age distribution
summary(hct$age)
hist(hct$age,
     main = "Age Distribution of HCT Attendees",
     ylab = "No. of Attendees",
     xlab = "Age of Attendees",
     axes = FALSE,
     ylim = c(0, 70),
     xlim = c(10, 80),
     col = "blue")
axis(1, las = 1)
custom_plot()

# Categorize age
hct$agecat <- cut(hct$age,   # Transform age variable into categories
                  breaks = c(0, 15, 20, 25, 50, 73),
                  include.lowest = TRUE,
                  labels = c("0 - 14", "15 - 19", "20 - 24", "25 - 49", "50+"))
head(hct$agecat)
agecat <- table(hct$agecat)
agecat
barplot(prop.table(agecat)*100,
        main = "Disribution of age (grouped data)",
        ylab = "Percentage",
        col = "orange",
        ylim = c(0, 80),
        axes = FALSE)
custom_plot()

# Match the age group with sex
contab <- table(hct$sex, hct$agecat)
contab
barplot(contab,
        beside = TRUE,
        legend = TRUE,
        col = c("pink", "blue"),
        main = "Sex vs. Age of those tested",
        ylab = "No. of Attendees",
        xlab = "Age Category",
        axes = FALSE)
custom_plot()

chisq.test(contab)
rm(contab)

# Marital Status
marital <- table(hct$marital.status)
round(percent(hct$marital.status), 2)
barplot(percent(hct$marital.status),
        main = "Marital Status of HCT attendees",
        col = "yellow",
        ylab = "Percentage",
        ylim = c(0, 80),
        axes = FALSE)
custom_plot()


# Knowledge Assessment

# Pregnant?
table(hct$pregnant)
percent(hct$pregnant)
barplot(percent(hct$pregnant),
        main = "Pregnant women",
        ylab = "Percentage",
        axes = FALSE,
        col = "pink")
custom_plot()

# Previous negative test
negative <- table(hct$prev.negative)
negative
barplot(prop.table(negative)*100,
        main = "Previously tested HIV-negative",
        ylim = c(0, 60),
        col = "orange",
        axes = FALSE)
custom_plot()

# Informed on transmission routes
routes <- hct$info.routes
prop.table(table(routes))*100
# Informed on risk factors
risk <- hct$info.risk
table(risk)
prop.table(table(risk))*100
# Informed on transmission methods
methods <- hct$info.methods
table(methods)
# Informed on possible results
resultinfo <- hct$info.result
table(resultinfo)
# Informed consent given
consent <- hct$informed.consent
table(consent)


# Combined plot on Knowledge Assessment
layout(matrix(c(1:4), nrow = 2, byrow = TRUE))
barplot(prop.table(table(routes))*100,
        col = "green",
        main = "Informed on transmission routes",
        ylab = "Percentage",
        axes = FALSE)
custom_plot()
barplot(prop.table(table(risk))*100,
        col = "green",
        main = "Informed on risk factors",
        axes = FALSE)
custom_plot()
barplot(prop.table(table(methods))*100,
        col = "green",
        main = "Informed on transmission methods",
        axes = FALSE)
custom_plot()
barplot(prop.table(table(resultinfo))*100,
        col = "green",
        main = "Informed on possible results",
        axes = FALSE)
custom_plot()
layout(matrix(1))

barplot(prop.table(table(consent))*100,
        col = "lightblue",
        main = "Informed consent given for testing",
        ylab = "Percentage",
        ylim = c(0, 100),
        axes = FALSE)
custom_plot()

length(which(consent == "No"))  # No. of persons who refused consent
rm(consent, methods, resultinfo, risk, routes)


# HIV Risk Assessment
# Combined plot
layout(matrix(1:6, nrow = 3, byrow = TRUE))
intercourse <- table(hct$sex.intercourse)
intercourse
barplot(intercourse, main = "Ever had sexual intercourse?")
custom_plot()
transfusion <- table(hct$transfusion)
barplot(transfusion,
        main = "Blood transfusion in the last 3 months")
custom_plot()
casual <- table(hct$casual.sex)
casual
barplot(prop.table(casual)*100,
        main = "Unprotected sex with casual partner, last 3 months",
        ylim = c(0, 100))
grid(NA, ny = NULL, lty = "dashed")
regular <- table(hct$regular.sex)
regular
barplot(prop.table(regular)*100,
        axes = FALSE,
        main = "Unprotected sex with regular partner, last 3 months")
custom_plot()
sti <- table(hct$sti)
barplot(prop.table(table(hct$sti))*100,
        main = "STI in the last 3 months",
        ylim = c(0, 100),
        axes = FALSE)
custom_plot()
multi <- table(hct$multi.partners)
barplot(prop.table(table(hct$multi.partners))*100,
        main = "More than 1 sex partner in last 3 months",
        ylim = c(0, 100),
        axes = FALSE)
custom_plot()
layout(matrix(1))

# match age with history of sexual intercourse
layout(matrix(1:2, nrow = 1, byrow = TRUE))
barplot(table(hct$sex.intercourse, hct$agecat),
        main = "Sexual exposure vs. Age",
        ylab = "No. of persons",
        beside = TRUE,
        legend = FALSE,
        col = c("red", "yellow"),
        axes = FALSE,
        ylim = c(0, 100))
custom_plot()
legend("topleft", legend = c("No", "Yes"), fill = c("red", "yellow"), col = c("red", "yellow"))
barplot(prop.table(table(hct$sex.intercourse, hct$agecat),2)*100,
        main = "Sexual exposure vs. Age",
        ylab = "Percentage",
        col = c("red", "yellow"),
        legend = TRUE,
        axes = FALSE,
        ylim = c(0, 100))
custom_plot()
layout(matrix(1))

# match marital status with histor of unprotected sex with casual partner
extramarital <- table(hct$casual.sex, hct$marital.status)
extramarital
prop.table(extramarital, 2)*100
barplot(extramarital,
        main = "Casual sex vs marital status",
        axes = FALSE)
custom_plot()

# match sex with history of unprotected sex with casual partner
extra_gender <- table(hct$casual.sex, hct$sex)
extra_gender
prop.table(extra_gender, 2)*100
barplot(prop.table(extra_gender, 2)*100,
        beside = TRUE,
        legend = TRUE,
        ylab = "Percentages",
        main = "Unprotected casual sex vs. gender",
        axes = FALSE)
custom_plot()


# Clinical TB Screening
layout(matrix(1:6, nrow = 3, byrow = TRUE))
coughing <- table(hct$coughing)
prop.table(coughing)
barplot(prop.table(coughing)*100,
        main = "Coughing > 2 weeks",
        ylim = c(0, 100),
        axes = FALSE)
custom_plot()

wtloss <- table(hct$wt.loss)
wtloss
prop.table(wtloss)
barplot(prop.table(wtloss)*100,
        main = "Weigh loss in last 4 weeks",
        axes = FALSE)
custom_plot()

lymph <- table(hct$lymphadeno)
lymph
barplot(lymph,
        axes = FALSE)
custom_plot()

fever <- table(hct$fever)
fever
barplot(fever,
        main = "Had fever more than 2 weeks",
        axes = FALSE)
custom_plot()

sweat <- table(hct$sweat)
sweat
barplot(sweat,
        main = "Had night sweats more than 2 weeks",
        axes = FALSE)
layout(matrix(1))

# Syndromic STI Screening
layout(matrix(c(1:6), nrow = 2, byrow = TRUE))

vag <- table(hct$vag.discharge)
vag
barplot(vag,
        main = "Vaginal discharge/burning while urinating",
        axes = FALSE)
custom_plot()

abd <- table(hct$lower.abd)
abd
barplot(abd,
        main = "Lower abdominal pain (female)",
        axes = FALSE)
custom_plot()

ureth <- table(hct$ureth.discharge)
ureth
barplot(ureth,
        main = "Urethral discharge (male)",
        axes = FALSE)
custom_plot()

scrotal <- table(hct$scrotal)
scrotal
barplot(scrotal,
        main = "Scrotal swelling/pain",
        axes = FALSE)
custom_plot()

sore <- table(hct$gen.sore)
sore
barplot(sore,
        main = "Genital sore/swollen ingunal nodes",
        axes = FALSE)
custom_plot()
layout(matrix(1))

rm(list = ls())

