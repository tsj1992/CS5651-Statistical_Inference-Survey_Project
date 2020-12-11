data <- read.csv(file="D:\\MSc\\Semester 3\\Statistical Inference\\Questionnaire\\CS5651-Statistical_Inference-Survey_Project\\responses\\209338R - Project Data - Preprocessed.csv", header=TRUE, sep=",")

sample_size = nrow(data)

dailyCount = nrow(data[data$On.average..how.often.do.you.use.Mobile.Banking.Apps.in.your.smartphone. == 'Daily',])
weeklyCount = nrow(data[data$On.average..how.often.do.you.use.Mobile.Banking.Apps.in.your.smartphone. == 'Weekly',])
sevTiMonthCount = nrow(data[data$On.average..how.often.do.you.use.Mobile.Banking.Apps.in.your.smartphone. == 'Several Times a month',])
monthlyCount = nrow(data[data$On.average..how.often.do.you.use.Mobile.Banking.Apps.in.your.smartphone. == 'Once a month',])
hardlyCount = nrow(data[data$On.average..how.often.do.you.use.Mobile.Banking.Apps.in.your.smartphone. == 'Hardly',])

category_count = 5

exp_val_for_each_cat = sample_size/category_count
exp_val_for_each_cat


e = exp_val_for_each_cat

((e-dailyCount)^2 + (e-weeklyCount)^2 + (e-sevTiMonthCount)^2 + (e-monthlyCount)^2 + (e-hardlyCount)^2)/e


category_count_vec <- c(dailyCount, weeklyCount, sevTiMonthCount, monthlyCount, hardlyCount)

chisq.test(category_count_vec)
