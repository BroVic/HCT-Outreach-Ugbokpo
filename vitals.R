# Ugbokpo Health & Wellness Check data

# Load the data
vital <- read.csv("VitalSigns.csv", na.strings = "", stringsAsFactors = FALSE)

# Overview of dataset
dim(vital)
str(vital)
View(vital) # there are additional 3 columns with spillover of some entries. 
#subset them
extracol <- vital[, 11:13]
extracol$NAs <- apply(extracol, 1, function(x) sum(is.na(x))) #NAs by rows


dim(vital)
colnames(vital) <- c("id", "date", "name", "sex", "temp", "pulse", "sys", "dia", "wt", "impress")
str(vital)

vital$date <- substr(vital$date, start = 1, stop = 10) #drop time notation
vital$date
vital$date <- factor(vital$date,
                     levels = c("08/12/2015", "09/12/2015", "10/12/2015"),
                     labels = c("Day 1", "Day 2", "Day 3"),
                     ordered = TRUE)
vital$sex <- as.factor(vital$sex)

vital[, 5:9] <- apply(vital[, 5:9], 2, as.numeric)
str(vital)

# Deal with missing values
apply(vital, 2, function(x){
  sum(is.na(x))
})

Amelia::missmap(vital)

vital$temp[is.na(vital$temp)] <- mean(vital$temp, trim = 0.5, na.rm = TRUE)
vital$pulse[is.na(vital$pulse)] <- mean(vital$pulse, trim = 0.5, na.rm = TRUE)
vital$sys[is.na(vital$sys)] <- mean(vital$sys, trim = 0.5, na.rm = TRUE)
vital$dia[is.na(vital$dia)] <- mean(vital$dia, trim = 0.5, na.rm = TRUE)
vital$wt[is.na(vital$wt)] <- mean(vital$wt, trim = 0.5, na.rm = TRUE)

vital$impress[is.na(vital$impress)] <- "Nil"

vital <- vital[complete.cases(vital), ]

apply(vital, 2, function(x){
  sum(is.na(x))
})



# Remove records with extreme error values
str(vital)
summary(vital)
tail(sort(vital$temp))
vital <- vital[which(vital$temp < 76), ]
dim(vital)

head(sort(vital$pulse))
tail(sort(vital$pulse))
vital <- vital[-which(vital$pulse == 158), ]

head(sort(vital$dia), n = 10L)
tail(sort(vital$dia), n = 10L)
vital <- vital[-which(vital$dia > 143), ]
dim(vital)


# Exploratory Analysis
summary(vital)
plot(vital[, c(6:9)])

# Date of Visit
date <- vital$date
table(date)
barplot(table(date))


# Sex
sex <- vital$sex
table(sex)
barplot(table(sex))

# Temperature
temp <- vital$temp
summary(temp)
hist(temp)

# Pulse
pulse <- vital$pulse
summary(pulse)
hist(pulse)

# systolic Pressure
systolic <- vital$sys
summary(systolic)
hist(systolic)

# Diastolic Pressure
diastolic <- vital$dia
summary(diastolic)
hist(diastolic)

# Weight
weight <- vital$wt
summary(weight)
hist(weight)

# Clinical Impression
diagnosis <- vital$impress


# hypertensives
hpt <- vital[which(vital$sys >= 140 & vital$dia >= 90), ]
summary(hpt)
plot(hpt[, c(6:9)])

