#!/usr/bin/env Rscript

suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(Rmisc)))

print_histogram <- function (df, title, xlab, col, filename, bins=15, xlimits=c(-10,10), density=F, printPlot=NA, ret=F, is.log=F) {
  
  num.class = length(names(table(df$class)))
  
  if (is.log) {
    df[,1] = log2( df[,1])
    xlab = paste("log2(", xlab,")")
  }
  
  if (density) {
    fnc = geom_density
  } else {
    fnc = geom_histogram
  }
  if (num.class == 1) {
    p1 <- ggplot(df, aes(x = value)) +  xlab(xlab) + ggtitle(title)
    p1 <- p1 + fnc(fill=col, size=.2, alpha = 0.2)  # aes(fill=factor(class))
    p1
  } else {
    p1 <- ggplot(df, aes(x=value)) +  xlab(xlab) + ggtitle(title)
    p1 <- p1 + fnc(aes(fill=color), size=.2, alpha = 0.2)
    p1 <- p1 + scale_fill_manual(labels = levels(df$class), values=col) 
    p1
  }
  if (length(xlimits) == 2) p1  <- p1 +  xlim(xlimits)
  
  if (!ret) multiplot(p1)  
  
  if (!is.na(printPlot) && filename!="") {
    filename = paste(dir_result, filename, sep="")
    ggsave(filename, dpi=dpi)
  }
  
  if (ret) return (p1)
}


# data1<-thetasHealth[,"median"];title1<-"CONTROL Median Distribution";xlab1="median";col1="blue";density=F
# data2<-thetasHealth[,"mean"]; title2<-"CONTROL mean Distribution";xlab2="mean";col2="red"
# xlimits1=NA; xlimits2=NA; see_mean_median=T; isLog=F; bins=30

# data1 <- z1; data2 <- z2; density=T; break_title=F
print_histogram2 <- function (data1, title1, xlab1, col1, data2, title2, xlab2, col2, filename, 
                              bins=15, xlimits1=NA, xlimits2=NA,density=F, printPlot=NA, isLog=F, 
                              see_mean_median=F,break_title=F) {
  if (!is.na(printPlot) && filename!="") {
    filename = paste(dir_result, filename, sep="")
    if (printPlot=="pdf") {
      pdf(filename) 
    } else {
      png(filename, width=10*dpi, height=8*dpi, res=dpi) 
    }
    
  }
  # mypar(1,2)
  # hist(thetasHealth[,"median"],breaks=20,main="CONTROL Median Distribution")
  # hist(thetasHealth[,"mean"],breaks=20,main="CONTROL mean Distribution")
  # mypar(1,1)
  df1 = data.frame(data1, 0)
  df2 = data.frame(data2, 0) 
  
  if (see_mean_median) {
    mu1 = round(mean(data1),3)
    median1 = round(median(data1),3)
    sd1 = round(sd(data1),3)
    
    if (break_title) title1 <- paste(title1, "\n mean =", mu1, "\n median =", median1, "\n SD =", sd1)
    else             title1 <- paste(title1, "\n mean =", mu1, ", median =", median1, ", SD =", sd1)
    
    mu2 = round(mean(data2),3)
    median2 = round(median(data2),3)
    sd2 = round(sd(data2),3)
    
    if (break_title) title2 <- paste(title2, "\n mean =", mu2, "\n median =", median2, "\n SD =", sd2)
    else             title2 <- paste(title2, "\n mean =", mu2, ", median =", median2, ", SD =", sd2)
  } 
  # head(df1)
  
  if (density) {
    p1 <- ggplot(df1) +  xlab(xlab1) + ggtitle(title1)
    if(!is.na(xlimits1)) p1 <- p1 + xlim(xlimits1)
    p1 <- p1 + stat_density(aes(data1), col=col1, alpha = 0.5)
    
    if (see_mean_median) {
      p1 <- p1 + geom_vline(xintercept=mu1, color="magenta4")
      p1 <- p1 + geom_vline(xintercept=median1, color="linen")
    }
    
    p2 <- ggplot(df2) +  xlab(xlab2) + ggtitle(title2)
    if(!is.na(xlimits2)) p2 <- p2 + xlim(xlimits2)
    p2 <- p2  + stat_density(aes(data2), col=col2, alpha = 0.5)
    
    if (see_mean_median) {
      p2 <- p2 + geom_vline(xintercept=mu2, color="magenta4")
      p2 <- p2 + geom_vline(xintercept=median2, color="linen")
    }
  } else {
    p1 <- ggplot(df1) +  xlab(xlab1) + ggtitle(title1)
    if(!is.na(xlimits1)) p1 <- p1 + xlim(xlimits1)
    p1 <- p1 + geom_histogram(aes(data1), bins=bins, col="white", fill=col1, alpha = 0.5)
    
    if (see_mean_median) {
      p1 <- p1 + geom_vline(xintercept=mu1, color="magenta4")
      p1 <- p1 + geom_vline(xintercept=median1, color="linen")
    }
    
    if (isLog) {
      p1 <- p1 + scale_y_log10() + ylab("log10(count)")
    }
    
    p2 <- ggplot(df2) +  xlab(xlab2) + ggtitle(title2)
    if(!is.na(xlimits2)) p2 <- p2 + xlim(xlimits2)
    p2 <- p2 + geom_histogram(aes(data2), bins=bins, col="white", fill=col2, alpha = 0.5)
    
    if (isLog) {
      p2 <- p2 + scale_y_log10() + ylab("log10(count)")
    }
    if (see_mean_median) {
      p2 <- p2 + geom_vline(xintercept=mu2, color="magenta4")
      p2 <- p2 + geom_vline(xintercept=median2, color="linen")
    }
    
  }
  
  if (!is.na(printPlot) && filename!="") {
    multiplot(p1,p2,layout=matrix(c(1,2),ncol=2, byrow=T))
    dev.off()
  } 
  (multiplot(p1,p2,layout=matrix(c(1,2),ncol=2, byrow=T)) )
}



# title = "d1 x d2"; xlab="x"; col1="navy"; col2="red"
print_2densities <- function(d1, d2, title = "d1 x d2", classes=c("d1", "d2"), xlab="x", col1="navy", col2="red") {

  df = rbind(  data.frame(val=d1, class="d1"), data.frame(val=d2, class="d2"))
  mu1 = mean(d1); mu2 = mean(d2)
  
  p1 <- ggplot(df, aes(val, fill=class)) +  xlab(xlab) + ggtitle(title) +
        geom_density(alpha = 0.3) +
        geom_vline(xintercept=mu1, color=col1) +
        geom_vline(xintercept=mu2, color=col2) +
        scale_fill_manual(labels=classes, values=c(col1,col2)) 
    
  multiplot(p1)
}


# isLog=F
doRoc <- function(xH0, xHa, lableH0, lableHa, class0, classX, xlab="sMDP", isLog=F, titleAux="") {
  
  maxi2sens <- 0; maxi2spec <- 0; maxi2soma <- 0; maxi2cutoff <- 0
  
  if (isLog) {
    xH0 <- log10(xH0)
    dH0 <- data.frame(x=xH0,class=class0)
    xHa <- log10(xHa)
    dHa <- data.frame(x=xHa,class=classX)
  } else {
    dH0 <- data.frame(x=xH0,class=class0)
    dHa <- data.frame(x=xHa,class=classX)
  }
  
  listROC <- contigency_table(xH0, xHa)
  
  spec <- listROC[["bestSpec"]]
  sens <- listROC[["bestSens"]]
  soma <- listROC[["maxi"]]
  bestCutoff <- listROC[["bestCutoff"]]
  
  if (soma > maxi2soma) {
    maxi2sens <- round(sens,2)
    maxi2spec <- round(spec,2)
    maxi2soma <- round(soma,2)
    maxi2cutoff <- round(bestCutoff,2)
  }
  
  titleDist <- paste("ROC:", lableHa, "x",lableH0)
  if (titleAux != "") {
    titleDist = paste(titleDist,"\n", titleAux)
  }
  titleROC  <- paste("best cutoff=", maxi2cutoff, 
                     ", sens=",maxi2sens, ", spec=",maxi2spec,
                     " (sum=",maxi2soma,")", sep="")
  
  dfROC <- data.frame(sensitivityList=listROC[["sensitivityList"]],
                      specificityList=listROC[["specificityList"]])
  
  dfBoth <- rbind(dH0, dHa)
  
  p1 <- ggplot(dfBoth, aes(x, fill=class)) + 
    geom_density(alpha = 0.4) + 
    scale_fill_manual(labels = levels(dfBoth$class),
                      values = c("dodgerblue3", "tomato")) +
    ggtitle(titleDist) + xlab(xlab) + 
    geom_vline(xintercept=maxi2cutoff, color="red") +
    annotate("text", x=maxi2cutoff ,
             y=7.5, label=paste("cutoff =",maxi2cutoff), size=4) +
    theme(legend.title=element_blank())
  # guides(fill=guide_legend(title=NULL))
  
  p2 <- ggplot(dfROC, aes(x=(1-sensitivityList), y=specificityList), alpha=1) + xlim(c(0,1)) + ylim(c(0,1)) +
    xlab("1-sensitivity") + ylab("specificity") + ggtitle(titleROC) + 
    geom_point()  + geom_line()  +
    geom_hline(yintercept=maxi2spec, color="red",linetype="dashed") +
    geom_vline(xintercept=1-maxi2sens, color="red",linetype="dashed") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1) )
  
  
  lista = list()
  lista[["p1"]] = p1
  lista[["p2"]] = p2
  lista[["best_sens"]] = maxi2sens
  lista[["best_spec"]] = maxi2spec
  lista[["cutoff"]] = maxi2cutoff
  
  return (lista)
}


# listH0 <- xH0; listHa <- xHa
contigency_table <- function(listH0, listHa) {
  
  all <- c(listH0, listHa)
  delta = (max(all) - min(all)) / 50
  xs <- seq(min(all)+delta,  max(all)-delta, delta)
  # c(max(all), min(all))
  # xs
  
  sensitivityList=c(); specificityList=c()
  
  maxi <- 0
  bestCutoff <- -99
  
  for (cutoff in xs) {
    TP = sum(listH0 <= cutoff)
    FP = sum(listH0 >  cutoff)
    TN = sum(listHa >= cutoff)
    FN = sum(listHa <  cutoff)
    # c(TP, FP, TN, FN)
    
    sens = TP / (TP + FN)
    spec = TN / (TN + FP)
    
    sensitivityList <- c(sensitivityList, c(sens))
    specificityList <- c(specificityList, c(spec))
    
    if (sens + spec > maxi)  {
      maxi <- sens + spec
      bestCutoff <- cutoff
      bestSens <- sens
      bestSpec <- spec
    } 
  }
  
  return (list(sensitivityList=sensitivityList, 
               specificityList=specificityList, 
               maxi=maxi, bestCutoff=bestCutoff, 
               bestSens=bestSens, bestSpec=bestSpec))
  
}


# exp=data; classX=c("one","two"); cols=c(1,2); title="normal distribution"; ylab="%"
# colors=c("blue", "red"); is.log=F; ylim=NA

my.boxplot <- function(exp, classX, cols, title, ylab, colors, is.log=F,ylim=NA) {
  
  data <- exp[,cols]
  if (is.log) data <- log10(data)
  
  df = data.frame()
  for (i in 1:ncol(data)) {
    stri = colnames(data)[i]
    df <- rbind(df, data.frame(val = data[,i], col=stri))
  }
  head(df)
  tail(df)
  
  # compare different sample populations across various temperatures
  p <-  ggplot(df, aes(x=col, y=val, fill=col)) +
    geom_boxplot(alpha=.4) + ggtitle(title) + ylab(ylab) + xlab("") +
    # geom_hline(yintercept=mu1, color=colors[1]) +  # ,linetype="dashed"
    # geom_hline(yintercept=mu1, color=colors[2]) +  # ,linetype="dashed"
    # geom_hline(yintercept= (mu1 + 2*sd1), color="black") +
    # geom_hline(yintercept= (mu1 - 2*sd1), color="black") +
    # geom_hline(yintercept= (mu2 + 2*sd2), color="black") +
    # geom_hline(yintercept= (mu2 - 2*sd2), color="black") +
    geom_jitter(shape=16, position=position_jitter(0.2),color="navy",alpha=.3)  +
    scale_fill_manual("", values=colors) + theme(axis.text.x = element_blank()) +
    theme(legend.position="bottom")
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8,color="black"))
  
  
  if (!(is.na(ylim))) {
    p <- p + ylim(ylim)
  }
  
  multiplot(p)
}
