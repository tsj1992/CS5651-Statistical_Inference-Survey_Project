data <- read.csv(file="D:\\MSc\\Semester 3\\Statistical Inference\\Questionnaire\\CS5651-Statistical_Inference-Survey_Project\\responses\\209338R - Project Data - Preprocessed.csv", header=TRUE, sep=",")

usageFrequencyTable = table(data$On.average..how.often.do.you.use.Mobile.Banking.Apps.in.your.smartphone.)

pct <- round(usageFrequencyTable/sum(usageFrequencyTable)*100,2)
lbls <- paste(names(usageFrequencyTable),"\n", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
colors <- c('red', 'yellow', 'blue', 'pink','brown','green','violet')
pie(usageFrequencyTable, main="Rate of how often the users engage with mobile banking apps",labels = lbls, col=colors)

eduLevelTable = table(data$Education.up.to..)

pct <- round(eduLevelTable/sum(eduLevelTable)*100,2)
lbls <- paste(names(eduLevelTable),"\n", pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
colors <- c('red', 'yellow', 'blue', 'pink','brown','green','violet')
pie(eduLevelTable, main="Education Level",labels = lbls, col=colors)

# bankingServices = table(data$What.type.of.bank.accounts.and.services.you.currently.engage.with..Select.all.that.applies.)
# barplot(bankingServices, main="Age Distribution", col=c("darkblue"),
        #xlab="Age (in years)",ylab="Frequency (No.of Users)")

ages = table(data$Age..in.years.)
barplot(ages, main="Age Distribution", col=c("darkblue"),
        xlab="Age (in years)",ylab="Frequency (No.of Users)")

appCount = table(data$How.many.Mobile.Banking.Apps.currently.setup.in.your.smartphone.)
barplot(appCount, main="No.of Mobile banking apps installed in users' phones", col=c("darkblue"),
        xlab="No.of Mobile banking apps installed in the smartphone", ylab="Frequency (No.of Users)")


