library(boot)

data <- read.csv(file="D:\\MSc\\Semester 3\\Statistical Inference\\Questionnaire\\CS5651-Statistical_Inference-Survey_Project\\responses\\209338R - Project Data - Preprocessed.csv", header=TRUE, sep=",")

totalCount = nrow(data)

meanStat <- function(data, indices){
  dt<-data[indices]
  return (mean(dt))
}

bootRCount = 9999

noneCount = nrow(data[data$Education.up.to.. == 'None',])
noneS = c(rep(1, noneCount), rep(0, totalCount-noneCount))
noneB = boot(noneS, meanStat, R=bootRCount)
plot(noneB)
print("BOOTSTRAP CONFIDENCE INTERVAL - NO FORMAL EDUCATION")
boot.ci(noneB, conf=0.95, type="bca")

less5Count = nrow(data[data$Education.up.to.. == 'Less than Grade 5',])
less5S = c(rep(1, less5Count), rep(0, totalCount-less5Count))
less5B = boot(less5S, meanStat, R=bootRCount)
plot(less5B)
print("BOOTSTRAP CONFIDENCE INTERVAL - EDUCATION LESS THAN GRADE 5")
boot.ci(less5B, conf=0.95, type="bca")

less11Count = nrow(data[data$Education.up.to.. == 'Less than Grade 11',])
less11S = c(rep(1, less11Count), rep(0, totalCount-less11Count))
less11B = boot(less11S, meanStat, R=bootRCount)
plot(less11B)
print("BOOTSTRAP CONFIDENCE INTERVAL - EDUCATION LESS THAN GRADE 11")
boot.ci(less11B, conf=0.95, type="bca")

gceOlCount = nrow(data[data$Education.up.to.. == 'G.C.E. (O/L)',])
gceOlS = c(rep(1, gceOlCount), rep(0, totalCount-gceOlCount))
gceOlB = boot(gceOlS, meanStat, R=bootRCount)
plot(gceOlB)
print("BOOTSTRAP CONFIDENCE INTERVAL - EDUCATION UP TO GCE O/L")
boot.ci(gceOlB, conf=0.95, type="bca")

aftOlDipCount = nrow(data[data$Education.up.to.. == 'After O/L Diploma',])
aftOlDipS = c(rep(1, aftOlDipCount), rep(0, totalCount-aftOlDipCount))
aftOlDipB = boot(aftOlDipS, meanStat, R=bootRCount)
plot(aftOlDipB)
print("BOOTSTRAP CONFIDENCE INTERVAL - AFTER O/L DIPLOMA EDUCATION")
boot.ci(aftOlDipB, conf=0.95, type="bca")

gceAlCount = nrow(data[data$Education.up.to.. == 'G.C.E. (A/L)',])
gceAlS = c(rep(1, gceAlCount), rep(0, totalCount-gceAlCount))
gceAlB = boot(gceAlS, meanStat, R=bootRCount)
plot(gceAlB)
print("BOOTSTRAP CONFIDENCE INTERVAL - EDUCATION UP TO GCE A/L")
boot.ci(gceAlB, conf=0.95, type="bca")

aftAlDipCount = nrow(data[data$Education.up.to.. == 'After A/L Diploma',])
aftAlDipS = c(rep(1, aftAlDipCount), rep(0, totalCount-aftAlDipCount))
aftAlDipB = boot(aftAlDipS, meanStat, R=bootRCount)
plot(aftAlDipB)
print("BOOTSTRAP CONFIDENCE INTERVAL - AFTER A/L DIPLOMA EDUCATION")
boot.ci(aftAlDipB, conf=0.95, type="bca")

bscCount = nrow(data[data$Education.up.to.. == 'Bachelors Degree',])
bscS = c(rep(1, bscCount), rep(0, totalCount-bscCount))
bscB = boot(bscS, meanStat, R=bootRCount)
plot(bscB)
print("BOOTSTRAP CONFIDENCE INTERVAL - FIRST DEGREE EDUCATION")
boot.ci(bscB, conf=0.95, type="bca")

mscCount = nrow(data[data$Education.up.to.. == 'Masters Degree',])
mscS = c(rep(1, mscCount), rep(0, totalCount-mscCount))
mscB = boot(mscS, meanStat, R=bootRCount)
plot(mscB)
print("BOOTSTRAP CONFIDENCE INTERVAL - MASTERS DEGREE EDUCATION")
boot.ci(mscB, conf=0.95, type="bca")

docCount = nrow(data[data$Education.up.to.. == 'Doctors Degree',])
docS = c(rep(1, docCount), rep(0, totalCount-docCount))
docB = boot(docS, meanStat, R=bootRCount)
plot(docB)
print("BOOTSTRAP CONFIDENCE INTERVAL - DOCTORS DEGREE EDUCATION")
boot.ci(docB, conf=0.95, type="bca")
