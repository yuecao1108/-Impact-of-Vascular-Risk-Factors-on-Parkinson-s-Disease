
X.raw = data.frame(Gender = 1, HTN_meds = 0,
               	   Age = 32, avg_SBP = 125, Smoke_Ever = 0,
                   BMI = 22.4, diabetes = 0)


Fram <- function(X.raw){
Framingham <- rep(NA, nrow(X.raw))
for(i in 1:nrow(X.raw)){
X <- X.raw[i,]
X$Age = log(X.raw[i,]$Age)
X$avg_SBP = log(X.raw[i,]$avg_SBP)
X$BMI = log(X.raw[i,]$BMI)

beta.male = data.frame(Age = 3.11, avg_SBP.NoTRT = 1.85, avg_SBP.TRT = 1.93,
                       Smoke_Ever = 0.71, BMI = 0.79, diabetes = 0.53)
beta.female = data.frame(Age = 2.72, avg_SBP.NoTRT = 2.81, avg_SBP.TRT = 2.88,
                         Smoke_Ever = 0.62, BMI = 0.51, diabetes = 0.78)

if (X$Gender == 0) {
  Xbeta = sum(X[, c('Age', 'Smoke_Ever', 'BMI', 'diabetes')] *
              beta.male[, c('Age', 'Smoke_Ever', 'BMI', 'diabetes')])
  if (X$HTN_meds > 0) {
    Xbeta = Xbeta + X$avg_SBP * beta.male$avg_SBP.TRT
  } else {
    Xbeta = Xbeta + X$avg_SBP * beta.male$avg_SBP.NoTRT
  }
  Framingham[i] = 1 - 0.88431 ^ exp(Xbeta - 23.9388)
} else {
  Xbeta = sum(X[, c('Age', 'Smoke_Ever', 'BMI', 'diabetes')] *
              beta.female[, c('Age', 'Smoke_Ever', 'BMI', 'diabetes')])
  if (X$HTN_meds > 0) {
    Xbeta = Xbeta + X$avg_SBP * beta.female$avg_SBP.TRT
  } else {
    Xbeta = Xbeta + X$avg_SBP * beta.female$avg_SBP.NoTRT
  }
  Framingham[i] = 1 - 0.94833 ^ exp(Xbeta - 26.0145)
}
}
return(Framingham)
}

Fram(X.raw)
