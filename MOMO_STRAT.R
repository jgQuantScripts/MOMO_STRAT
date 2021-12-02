require("PerformanceAnalytics");require("quantmod");require("pbapply"); require("data.table")


e <- new.env()
tickers <- c("AMZN","BIDU","GLD","GOOGL","GS","IWM","NFLX","MMM","DIA","SPY")
getSymbols(tickers,from="2003-01-01",env=e)
PRC <- do.call(merge,eapply(e,Ad))

if(last(index(PRC)) != Sys.Date())
{
  last <- pblapply(as.list(gsub(".Adjusted","",names(PRC))), getQuote)
  PRC <- rbind(PRC,xts(coredata(t(rbindlist(last)$Last)),order.by=Sys.Date()))
}

NOM <- colnames(PRC) <- gsub(".Adjusted","",names(PRC))

MOMO60 <- round(ROC(PRC,n=60,type="discrete"),4)

MOMO60 <- MOMO60["20030331::"]
PRC <- PRC["20030331::"]


indx <- seq(as.Date("2003-03-31"), length.out = 300, by='4 weeks')


SELECT <- MOMO60[paste(indx)];dim(SELECT)

indx2 <- ifelse((indx %in% index(SELECT) == FALSE), paste(indx+1),paste(indx))

SELECT <- MOMO60[paste(indx2)];dim(SELECT)
PRC2 <- PRC[paste(indx2)];dim(SELECT)

ASSETS4 <- combn(NOM,4)


MOMO = function(x)
{
  y <- ASSETS4[,x]
  S <- SELECT[,y]
  
  SEQ <- as.numeric(apply(S,1,which.max))
  
  prc2 <- round(PRC2[,y],2)
  RETS <- CalculateReturns(prc2,"discrete")
  
 ALL <- do.call(merge,lapply(as.list(1:ncol(RETS)), function(x){
   Lag(reclass(ifelse(SEQ==x,1,0),match.to = S)*RETS[,x])
 }))
  
  colnames(ALL) <- names(prc2)
  ALL[is.na(ALL)]<-0
  
  EQT <- reclass(rowSums(ALL),match.to=ALL); EQT[is.na(EQT)]<-0
  colnames(EQT) <- paste(names(prc2), collapse = "-")
  EQT
}

STRAT <- pblapply(as.list(1:ncol(ASSETS4)), function(x) MOMO(x))

AAA <- pblapply(STRAT,colSums)

df <- STRAT[order(sapply(AAA,"[[",1))]
df <- df[(length(df)-9):length(df)]
TOP10 <- do.call(merge,df)

charts.PerformanceSummary(TOP10, cex.legend=0.45, colorset=rich10equal, geometric = TRUE, main="TOP 10")
table.Stats(TOP10)
chart.RiskReturnScatter(TOP10, add.sharpe = c(1), Rf=(0.03/sqrt(252)),
                        colorset = rich10equal, xlim = c(0.45,0.55), ylim = c(1.4,1.75))

AAA <- lapply(df,colSums)
AAA[[which.max(AAA)]]

EQT <- df[[which.max(AAA)]]

charts.PerformanceSummary(EQT,geometric = TRUE)
table.Stats(EQT)
table.Drawdowns(EQT)











getMOMO = function(x)
{
  y <- as.character(strsplit(x,"-")[[1]])
  S <- SELECT[,y]
  
  SEQ <- as.numeric(apply(S,1,which.max))
  
  prc2 <- round(PRC2[,y],2)
  RETS <- CalculateReturns(prc2,"discrete")
  
  ALL <- do.call(merge,lapply(as.list(1:ncol(RETS)), function(x){
    Lag(reclass(ifelse(SEQ==x,1,0),match.to = S)*RETS[,x])
  }))
  
  colnames(ALL) <- names(prc2)
  ALL[is.na(ALL)]<-0
  
  EQT <- reclass(rowSums(ALL),match.to=ALL); EQT[is.na(EQT)]<-0
  colnames(EQT) <- "MoMoRet"
  cbind(prc2,SEQ,round(EQT,4))
}






