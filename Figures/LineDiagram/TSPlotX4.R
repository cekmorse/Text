setwd("~/Projects/Thesis/Figures-under-construction/TSDescription")

x <- seq(0, 2*pi, 0.01)
y <- sin(x)

dsy <- data.frame(matrix(sin(x), nrow=length(x), ncol=500))
noise <- data.frame(matrix(rnorm(length(x)*500, mean=0, sd=0.1), nrow=dim(dsy)[[1]]))
dsy.noise <- dsy+noise

dsy.mean = rowMeans(dsy)
dsy.upper = apply(dsy.noise, 1, quantile, probs=c(.975))
dsy.lower = apply(dsy.noise, 1, quantile, probs=c(.025))

pdf(file="TSDescription.pdf", family="Times")
par(mar=c(5.1,4.1,1.1,1.1))
par(mfrow=c(4,1))
plot(x,y, ylim = c(-1.5,1.5), ylab="Value", xlab="Time", xaxt="n", yaxt="n", type="l")
plot(x,dsy.noise[,1], ylim = c(-1.5,1.5), ylab="Value", xlab="Time", xaxt="n", yaxt="n", type="l")
plot(x,y, col="chartreuse", ylim = c(-1.5,1.5), ylab="Value", xlab="Time", xaxt="n", yaxt="n", type="l")
for (i in 1:500) {
  lines(x, dsy.noise[,i], col="chartreuse")
}
plot(x,y, col="chartreuse", ylim = c(-1.5,1.5), ylab="Value", xlab="Time", xaxt="n", yaxt="n", type="l")
for (i in 1:500) {
  lines(x, dsy.noise[,i], col="chartreuse")
}
lines(x, dsy.mean)
lines(x, dsy.upper)
lines(x, dsy.lower)
dev.off()
