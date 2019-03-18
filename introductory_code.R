library(stats)

### plot different types of Beta dists
p = seq(0,1, length=100)
plot(p, dbeta(p, 15, 2), ylab="density", type ="l", col=4)
lines(p, dbeta(p, 8, 4), type ="l", col=3)
lines(p, dbeta(p, 4, 4), type ="l", col=2)
legend(0.4, 5,c("Be(15,2) - ICE","Be(8,4) - RE/RB","Be(4,4) - SBahn"),lty=c(1,1),col=c(4,3))

