# NACA HcT & Wellness Community Outreach, Ugbokpo, Apa LGA, Benue State Nigeria
# December 2015
# ----------------------------------------------------------------------------
# HIV Counselling and Testing (hctdata) Data
# ---------------------------------------------------------------------------

# ====================
# ExPLORATORY ANALYSIS
# ====================

# Local functions
custom_plot <- function(){ # adjusts y-axis and adds horizontal gridlines
  axis(2, las = 2)
  grid(NA, ny = NULL, col = "lightgrey", lty = "dashed")
}

percent <- function(x) prop.table(table(x))*100 # prop.table as percent values
# -----------------------------------------------------------------------------

# Generate an overview/summary of the data
print(summary(hctdata))

# No. of persons counselled and tested
days <- table(hctdata$date)
days
barplot(days,
        main = "No. of persons counselled and tested",
        ylim = c(0, 120),
        col = "blue",
        ylab = "No. of persons",
        axes = FALSE)
custom_plot()

# Age distribution
summary(hctdata$age)
hist(hctdata$age,
     main = "Age Distribution of hctdata Attendees",
     ylab = "No. of Attendees",
     xlab = "Age of Attendees",
     axes = FALSE,
     ylim = c(0, 70),
     xlim = c(10, 80),
     col = "blue")
axis(1, las = 1)
custom_plot()

# Categorize age
hctdata$agecat <- cut(hctdata$age,   # Transform age variable into categories
                  breaks = c(0, 15, 20, 25, 50, 73),
                  include.lowest = TRUE,
                  labels = c("0 - 14", "15 - 19", "20 - 24", "25 - 49", "50+"))
head(hctdata$agecat)
agecat <- table(hctdata$agecat)
agecat
barplot(prop.table(agecat)*100,
        main = "Disribution of age (grouped data)",
        ylab = "Percentage",
        col = "orange",
        ylim = c(0, 80),
        axes = FALSE)
custom_plot()

# Match the age group with sex
contab <- table(hctdata$sex, hctdata$agecat)
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
marital <- table(hctdata$marital.status)
round(percent(hctdata$marital.status), 2)
barplot(percent(hctdata$marital.status),
        main = "Marital Status of hctdata attendees",
        col = "yellow",
        ylab = "Percentage",
        ylim = c(0, 80),
        axes = FALSE)
custom_plot()


# Knowledge Assessment

# Pregnant?
table(hctdata$pregnant)
percent(hctdata$pregnant)
barplot(percent(hctdata$pregnant),
        main = "Pregnant women",
        ylab = "Percentage",
        axes = FALSE,
        col = "pink")
custom_plot()

# Previous negative test
negative <- table(hctdata$prev.negative)
negative
barplot(prop.table(negative)*100,
        main = "Previously tested HIV-negative",
        ylim = c(0, 60),
        col = "orange",
        axes = FALSE)
custom_plot()

# Informed on transmission routes
routes <- hctdata$info.routes
prop.table(table(routes))*100
# Informed on risk factors
risk <- hctdata$info.risk
table(risk)
prop.table(table(risk))*100
# Informed on transmission methods
methods <- hctdata$info.methods
table(methods)
# Informed on possible results
resultinfo <- hctdata$info.result
table(resultinfo)
# Informed consent given
consent <- hctdata$informed.consent
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
intercourse <- table(hctdata$sex.intercourse)
intercourse
barplot(intercourse, main = "Ever had sexual intercourse?")
custom_plot()
transfusion <- table(hctdata$transfusion)
barplot(transfusion,
        main = "Blood transfusion in the last 3 months")
custom_plot()
casual <- table(hctdata$casual.sex)
casual
barplot(prop.table(casual)*100,
        main = "Unprotected sex with casual partner, last 3 months",
        ylim = c(0, 100))
grid(NA, ny = NULL, lty = "dashed")
regular <- table(hctdata$regular.sex)
regular
barplot(prop.table(regular)*100,
        axes = FALSE,
        main = "Unprotected sex with regular partner, last 3 months")
custom_plot()
sti <- table(hctdata$sti)
barplot(prop.table(table(hctdata$sti))*100,
        main = "STI in the last 3 months",
        ylim = c(0, 100),
        axes = FALSE)
custom_plot()
multi <- table(hctdata$multi.partners)
barplot(prop.table(table(hctdata$multi.partners))*100,
        main = "More than 1 sex partner in last 3 months",
        ylim = c(0, 100),
        axes = FALSE)
custom_plot()
layout(matrix(1))

# match age with history of sexual intercourse
layout(matrix(1:2, nrow = 1, byrow = TRUE))
barplot(table(hctdata$sex.intercourse, hctdata$agecat),
        main = "Sexual exposure vs. Age",
        ylab = "No. of persons",
        beside = TRUE,
        legend = FALSE,
        col = c("red", "yellow"),
        axes = FALSE,
        ylim = c(0, 100))
custom_plot()
legend("topleft", legend = c("No", "Yes"), fill = c("red", "yellow"), col = c("red", "yellow"))
barplot(prop.table(table(hctdata$sex.intercourse, hctdata$agecat),2)*100,
        main = "Sexual exposure vs. Age",
        ylab = "Percentage",
        col = c("red", "yellow"),
        legend = TRUE,
        axes = FALSE,
        ylim = c(0, 100))
custom_plot()
layout(matrix(1))

# match marital status with histor of unprotected sex with casual partner
extramarital <- table(hctdata$casual.sex, hctdata$marital.status)
extramarital
prop.table(extramarital, 2)*100
barplot(extramarital,
        main = "Casual sex vs marital status",
        axes = FALSE)
custom_plot()

# match sex with history of unprotected sex with casual partner
extra_gender <- table(hctdata$casual.sex, hctdata$sex)
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
coughing <- table(hctdata$coughing)
prop.table(coughing)
barplot(prop.table(coughing)*100,
        main = "Coughing > 2 weeks",
        ylim = c(0, 100),
        axes = FALSE)
custom_plot()

wtloss <- table(hctdata$wt.loss)
wtloss
prop.table(wtloss)
barplot(prop.table(wtloss)*100,
        main = "Weigh loss in last 4 weeks",
        axes = FALSE)
custom_plot()

lymph <- table(hctdata$lymphadeno)
lymph
barplot(lymph,
        axes = FALSE)
custom_plot()

fever <- table(hctdata$fever)
fever
barplot(fever,
        main = "Had fever more than 2 weeks",
        axes = FALSE)
custom_plot()

sweat <- table(hctdata$sweat)
sweat
barplot(sweat,
        main = "Had night sweats more than 2 weeks",
        axes = FALSE)
layout(matrix(1))

# Syndromic STI Screening
layout(matrix(c(1:6), nrow = 2, byrow = TRUE))

vag <- table(hctdata$vag.discharge)
vag
barplot(vag,
        main = "Vaginal discharge/burning while urinating",
        axes = FALSE)
custom_plot()

abd <- table(hctdata$lower.abd)
abd
barplot(abd,
        main = "Lower abdominal pain (female)",
        axes = FALSE)
custom_plot()

ureth <- table(hctdata$ureth.discharge)
ureth
barplot(ureth,
        main = "Urethral discharge (male)",
        axes = FALSE)
custom_plot()

scrotal <- table(hctdata$scrotal)
scrotal
barplot(scrotal,
        main = "Scrotal swelling/pain",
        axes = FALSE)
custom_plot()

sore <- table(hctdata$gen.sore)
sore
barplot(sore,
        main = "Genital sore/swollen ingunal nodes",
        axes = FALSE)
custom_plot()
layout(matrix(1))

rm(list = ls())

