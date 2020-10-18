# install.packages("fastICA")
# library(fastICA)
# install.packages("pls")
# library(pls)
RNGversion('3.5.1')
NIR <- read.csv2("NIRspectra.csv")

#Principal component analysis

NIR$Viscosity <- c()
res=prcomp(NIR)
lambda <- res$sdev^2
#eigenvalues
lambda

#displays variation captured of each feature
sprintf("%2.3f",lambda/sum(lambda)*100)

#plot histograms of variance
screeplot(res)

#scores of PC1 & PC2 coordinates
plot(res$x[,1], res$x[,2], main = "PC1 v PC2", xlab = "PC1", ylab = "PC2")


U <- res$rotation
plot(U[,1], main="Traceplot, PC1")
plot(U[,2],main="Traceplot, PC2")

#ICA
set.seed(12345)
ICA <- fastICA(NIR, 2)

wPrime <- ICA$K %*% ICA$W

plot(wPrime[,1], main="trace plot ICA1")
plot(wPrime[,2], main="trace plot ICA2")

plot(ICA$S[,1], ICA$S[,2], main = "Latent 1 v Latent 2", xlab ="Latent 1", ylab="Latent 2")
