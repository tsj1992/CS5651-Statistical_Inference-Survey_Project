library(boot)

data <- read.csv(file="D:\\MSc\\Semester 3\\Statistical Inference\\Questionnaire\\CS5651-Statistical_Inference-Survey_Project\\responses\\209338R - Project Data - Preprocessed.csv", header=TRUE, sep=",")

totalCount = nrow(data)

meanStat <- function(data, indices){
  dt<-data[indices]
  return (mean(dt))
}

bootRCount = 9999

dailyCount = nrow(data[data$On.average..how.often.do.you.use.Mobile.Banking.Apps.in.your.smartphone. == 'Daily',])
dailyS = c(rep(1, dailyCount), rep(0, totalCount-dailyCount))
dailyB = boot(dailyS, meanStat, R=bootRCount)
plot(dailyB)
print("BOOTSTRAP CONFIDENCE INTERVAL - DAILY USAGE")
boot.ci(dailyB, conf=0.95, type="bca")

weeklyCount = nrow(data[data$On.average..how.often.do.you.use.Mobile.Banking.Apps.in.your.smartphone. == 'Weekly',])
weeklyS = c(rep(1, weeklyCount), rep(0, totalCount-weeklyCount))
weeklyB = boot(weeklyS, meanStat, R=bootRCount)
plot(weeklyB)
print("BOOTSTRAP CONFIDENCE INTERVAL - WEEKLY USAGE")
boot.ci(weeklyB, conf=0.95, type="bca")

sevTiMonthCount = nrow(data[data$On.average..how.often.do.you.use.Mobile.Banking.Apps.in.your.smartphone. == 'Several Times a month',])
sevTiMonthS = c(rep(1, sevTiMonthCount), rep(0, totalCount-sevTiMonthCount))
sevTiMonthB = boot(sevTiMonthS, meanStat, R=bootRCount)
plot(sevTiMonthB)
print("BOOTSTRAP CONFIDENCE INTERVAL - SEVERAL TIMES A MONTH OF USAGE")
boot.ci(sevTiMonthB, conf=0.95, type="bca")

onceMonthCount = nrow(data[data$On.average..how.often.do.you.use.Mobile.Banking.Apps.in.your.smartphone. == 'Once a month',])
onceMonthS = c(rep(1, onceMonthCount), rep(0, totalCount-onceMonthCount))
onceMonthB = boot(onceMonthS, meanStat, R=bootRCount)
plot(onceMonthB)
print("BOOTSTRAP CONFIDENCE INTERVAL - ONCE A MONTH USAGE")
boot.ci(onceMonthB, conf=0.95, type="bca")

hardlyCount = nrow(data[data$On.average..how.often.do.you.use.Mobile.Banking.Apps.in.your.smartphone. == 'Hardly',])
hardlyS = c(rep(1, hardlyCount), rep(0, totalCount-hardlyCount))
hardlyB = boot(hardlyS, meanStat, R=bootRCount)
plot(hardlyB)
print("BOOTSTRAP CONFIDENCE INTERVAL - HARDLY USAGE")
boot.ci(hardlyB, conf=0.95, type="bca")

