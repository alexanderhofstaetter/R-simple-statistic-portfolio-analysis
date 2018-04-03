## @knitr init
require("tseries")
require("foreach")

con <- url("http://quote.yahoo.com")
#instrumente <- read.csv(file='instruments.csv',sep=',',stringsAsFactors=FALSE)

instrumente <- c("NESR.DE", "VOW3.DE", "AAPL")
namen <- c("Nestlé S.A.", "Volkswagen AG", "Apple Inc.")
boersen <- c("XETRA", "XETRA", "NASDAQ")

table <- data.frame(instrumente, paste(namen," (",boersen,")", sep=""))
colnames(table) <- c("Symbol","Bezeichnung")
start <- "2002-01-01"

# Rendite function
# Berechnet die diskreten Renditen eines Vektors
rendite <- function(S) {
  r <- (S[-1] - S[1:(length(S)-1)])/S[1:(length(S)-1)]
  return (r)
}

# Schiefekoeffizient
sk <- function(R) {
  rechts <- quantile(R)["75%"] - quantile(R)["50%"]
  links <- quantile(R)["50%"] - quantile(R)["25%"]
  sk <- as.numeric((rechts-links)/(rechts+links))
  return (sk)
}

# Wölbungskoeffizient
wk <- function(R) {
  QD <- as.numeric(quantile(R)["75%"] - quantile(R)["25%"])
  s <- sqrt(var(R))
  wk <- QD/s
  return (wk)
}


## @knitr kurse-und-renditen
x <- list()
r <- list()
foreach(i=1:length(instrumente)) %do% { 
  # Kurse
  ticker <- instrumente[i]
  x[[ticker]] <- get.hist.quote(instrument = ticker, quote = c("Cl", "Vol"), start=start) 
  
  # Renditen
  S <- as.numeric(x[[ticker]]$Close)
  r[[ticker]] <- rendite(S)
}

# Maximale Y Werte bestimmen (für Plot)
ymax <- max(x[[1]]$Close, na.rm=TRUE)
foreach(i=2:length(instrumente)) %do% { 
  ymax <- max(ymax,max(x[[i]]$Close, na.rm=TRUE))
}

## @knitr plot-kurse
# Alle Kurse plotten
plot(x[[1]]$Close, col=1, ylim=c(0, ymax), main="Kursverläufe", ylab="Kurs", xlab="Datum")
foreach(i=2:length(instrumente)) %do% { 
  lines(x[[i]]$Close, col=i) 
}
legend("topleft", legend = paste(namen," (",instrumente,")",", ",boersen, sep=""), col = c(1:4), lwd = c(1,2), lty = c(1,1) )


## @knitr plot-renditen
# Renditen plotten
par(mfrow=c(length(instrumente),1)) 
foreach(i=1:length(instrumente)) %do% { 
  plot(r[[i]], col=i, main=paste("Diskrete Renditen im Zeitverlauf", instrumente[i], ""), ylab="Rendite", xlab=paste("Tage", "seit", start))
}
par(mfrow=c(1,1))

R <- na.omit(r[[1]])

## @knitr analyse
# Schiefekoeffizient (siehe Statistikfolien 3)
## rechtschief/linksschief
rechts <- quantile(R)["75%"] - quantile(R)["50%"]
links <- quantile(R)["50%"] - quantile(R)["25%"]
#sk > 0: rechtschief: Mittelwert > Median
#sk < 0: linksschief: Mittelwert < Median
sk <- as.numeric((rechts-links)/(rechts+links)) ## normalverteilt: sk = 0

## Erwartungswert, Median, Varianz und Standardabweichung
mean <- mean(R)
median <- median(R)
var <- var(R)
s <- sqrt(var(R))

# Wölbungskoeffizient
QD <- as.numeric(quantile(R)["75%"] - quantile(R)["25%"]) # Interquartilsdistanz
wk <- QD/s 
# normalverteilt: wk = 1.34 (mesokurtisch) 
# längere Enden als Normalverteilung: WK < 1.34 (leptokurtisch)
# kürzere Enden als Normalverteilung: WK > 1.34 (platykurtisch)

## @knitr quantile
## empirische Quantile
quantile(R)
quantile(R, c(0.05, 0.95))

## @knitr plot-boxplot
par(mfrow=c(1,length(instrumente))) 
foreach(i=1:length(instrumente)) %do% { 
  boxplot(r[[i]], horizontal=TRUE, ylab=" ")
  title(paste("Boxplot Renditen", namen[i]))
}
par(mfrow=c(1,1)) 

## @knitr plot-dichte
par(mfrow=c(1,length(instrumente))) 
foreach(i=1:length(instrumente)) %do% { 
  plot(density(na.omit(r[[i]])), main=paste("Dichtefunktion Rendite", namen[i]), ylab="Dichte")
  abline(v = mean(na.omit(r[[i]])), col = "red", lwd=2, lty="dashed")
}
par(mfrow=c(1,1)) 


## @knitr plot-normalverteilung
par(mfrow=c(1,length(instrumente))) 
foreach(i=1:length(instrumente)) %do% { 
  qqnorm(na.omit(r[[i]]), main=paste("Normal Q-Q Plot", namen[i]), xlab="Theoretische Quantile", ylab="Quantile der Renditen")
  qqline(na.omit(r[[i]]))
}
par(mfrow=c(1,1)) 

## @knitr jbt
## Jarque Bera Test
# Tests the null of normality for x using the Jarque-Bera test statistic.
# This test is a joint statistic using skewness and kurtosis coefficients.
# grosser p-Wert: >> normalverteilt
# foreach(i=1:length(instrumente)) %do% { 
#   R <- na.omit(r[[i]])
#   jarque.bera.test(R)
# }

## @knitr plot-korrelation
ccoeff <- function(x1,x2) {
  s <- merge(x1,x2) 
  c <- cor(s[,"Close.x1"],s[,"Close.x2"],use="pairwise.complete.obs")
  return (format(c,digits=3))
}
correlation <- function(x,i1,i2) {
  x11 <- as.data.frame(x[[i1]])
  x22 <- as.data.frame(x[[i2]])
  
  x2new <- x22[rownames(x11),]
  x1new <- x11[rownames(x2new),]
  
  x1new2 <- x1new[!is.na(x1new$Close),]
  x2new2 <- x2new[rownames(x1new2),]
  
  S1 <- as.numeric(x1new2$Close)
  R1 <- rendite(S1)
  
  S2 <- as.numeric(x2new2$Close)
  R2 <- rendite(S2)
  
  plot(R1,R2, main=paste("c = ",ccoeff(x[[i1]],x[[i2]]),sep=""), ylab=namen[i1], xlab=namen[i2])
}
par(mfrow=c(1,length(instrumente))) 
correlation(x,1,2)
correlation(x,1,3)
correlation(x,2,3)
par(mfrow=c(1,1)) 