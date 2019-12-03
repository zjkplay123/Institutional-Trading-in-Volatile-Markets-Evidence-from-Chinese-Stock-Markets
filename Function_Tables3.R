#full paper link: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3378571

#Function of finding abnormal returns in subsequent days
Find_abnormal_return=function(x,y){
  x=x[,-1]
  x=subset(x,x$nodata==0)
  x$code=as.character(x$code)
  x$date=as.Date(x$date,format="%Y-%m-%d")
  y$code=as.character(y$code)
  y$date=as.Date(y$date,format="%Y-%m-%d")
  xx=subset(x,x$date %in% y$date)
  open2=c()
  close2=c()
  ab2=c()
  ab3=c()
  ab4=c()
  ab5=c()
  ab6_10=c()
  ab11_20=c()
  ab21_60=c()
  ab61_120=c()
  for (i in 1:length(xx[,1])) {
    w=subset(x,x$code==xx$code[i])
    w$abreturn=100*log(1+(w$abreturn/100))
    ll=length(w[,1])
    or=which(w$date==xx$date[i])
    open2[i]=w$open[or+1]
    close2[i]=w$close[or+1]
    ab2[i]=w$abreturn[or+2]
    ab3[i]=w$abreturn[or+3]
    ab4[i]=w$abreturn[or+4]
    ab5[i]=w$abreturn[or+5]
    if ((or+10)>ll){
      ab6_10[i]=NA
    } else {
      ab6_10[i]=sum(w$abreturn[or+6]:w$abreturn[or+10])
    }
    if ((or+20)>ll){
      ab11_20[i]=NA
    } else {
      ab11_20[i]=sum(w$abreturn[or+11]:w$abreturn[or+20])
    }
    if ((or+60)>ll){
      ab21_60[i]=NA
    } else {
      ab21_60[i]=sum(w$abreturn[or+21]:w$abreturn[or+60])
    }
    if ((or+120)>ll){
      ab61_120[i]=NA
    } else {
      ab61_120[i]=sum(w$abreturn[or+61]:w$abreturn[or+120])
    }
    
    if (i %%100==0){
      print(i)
    }
  }
  xx=data.frame(xx,open2,close2,ab2,ab3,ab4,ab5,ab6_10,ab11_20,ab21_60,ab61_120)
  return(xx)
}

#Function corresponding to Table 3 in paper
tfun2=function(xx){
  k=matrix(NA,nrow=13,ncol=11)
  x1=subset(xx,xx$return>=9.95)
  x2=subset(xx,xx$return>=9 & xx$return<9.95)
  x3=subset(xx,xx$return>=8 & xx$return<9)
  x4=subset(xx,xx$return>=7 & xx$return<8)
  x5=subset(xx,xx$return>=6 & xx$return<7)
  x6=subset(xx,xx$return>=5 & xx$return<6)
  x7=subset(xx,xx$return>=-5 & xx$return<=5)
  x8=subset(xx,xx$return>=-6 & xx$return<=-5)
  x9=subset(xx,xx$return>=-7 & xx$return<=-6)
  x10=subset(xx,xx$return>=-8 & xx$return<=-7)
  x11=subset(xx,xx$return>=-9 & xx$return<=-8)
  x12=subset(xx,xx$return>=-9.95 & xx$return<=-9)
  x13=subset(xx,xx$return<=-9.95)
  ag=list(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
  for (i in 1:13) {
    a1=ag[[i]]
    re1=(a1$open2-a1$close)/a1$close
    re2=(a1$close2-a1$open2)/a1$open2
    re1=na.omit(re1)
    re2=na.omit(re2)
    re1=100*(log(1+re1))
    re2=100*(log(1+re2))
    k[i,1]=tstar(re1)
    k[i,2]=tstar(re2)
    k[i,3]=tstar(na.omit(a1$ab2))
    k[i,4]=tstar(na.omit(a1$ab3))
    k[i,5]=tstar(na.omit(a1$ab4))
    k[i,6]=tstar(na.omit(a1$ab5))
    k[i,7]=tstar(na.omit(a1$ab6_10))
    k[i,8]=tstar(na.omit(a1$ab11_20))
    k[i,9]=tstar(na.omit(a1$ab21_60))
    k[i,10]=tstar(na.omit(a1$ab61_120))
    k[i,11]=length(a1[,1])
  }
  row.names(k)=c("Upper Hit","[9%,10%)","[8%,9%)","[7%,8%)","[6%,7%)","[5%,6%)","[-5%,5%)",
                 "[-6%,-5%)","[-7%,-6%)","[-8%,-7%)","[-9%,-8%)","(-10%,-9%)","Lower Hit")
  colnames(k)=c("Close to open","Open to close","Day 2","Day 3","Day 4","Day 5","[6,10]",
                "[11,20]","[21,60]","[61,120]","# of Hit")
  return(k)
}

#defind dummy variable involving price range
add_class_up=function(x){
  upper=c()
  eight=c()
  six=c()
  four=c()
  for (i in 1:length(x[,1])) {
    if (x$return[i]>=9.95){
      upper[i]=1
      eight[i]=0
      six[i]=0
      four[i]=0
    } else if (x$return[i]>=8 & x$return[i]<9.95) {
      upper[i]=0
      eight[i]=1
      six[i]=0
      four[i]=0
    }else if (x$return[i]>=6 & x$return[i]<8) {
      upper[i]=0
      eight[i]=0
      six[i]=1
      four[i]=0
    } else if (x$return[i]>=4 & x$return[i]<6) {
      upper[i]=0
      eight[i]=0
      six[i]=0
      four[i]=1
    } else {
      upper[i]=0
      eight[i]=0
      six[i]=0
      four[i]=0
    }
  }
  supper=c()
  for (i in 1:length(x[,1])){
    if (x$status[i]==2|x$status[i]==3|x$status[i]==4|x$status[i]==6){
      if (x$return[i]>=4.95){
        supper[i]=1
      } else {
        supper[i]=0
      }
    } else {
      supper[i]=0
    }
  }
  x=data.frame(x,upper,eight,six,four,supper)
  return(x)
}

#descriptive statistics
Function_descriptive=function(x){
  x$netbuy=x$netbuy*1000
  x$netsell=x$netsell*1000
  x$return=x$return/100
  k=matrix(NA,nrow=14,ncol=8)
  k[1,1:8]=c(summary(x$return),sd(x$return),length(x$return))
  k[2,1:8]=c(summary(x$own1),sd(x$own1),length(x$own1))
  k[3,1:8]=c(summary(x$soe),sd(x$soe),length(x$soe))
  k[4,1:8]=c(summary(x$wall),sd(x$wall),length(x$wall))
  k[5,1:8]=c(summary(x$wfund),sd(x$wfund),length(x$wfund))
  k[6,1:8]=c(summary(x$wQFII),sd(x$wQFII),length(x$wQFII))
  k[7,1:8]=c(summary(x$wfinance),sd(x$wfinance),length(x$wfinance))
  k[8,1:8]=c(summary(x$wother),sd(x$wother),length(x$wother))
  k[9,1:8]=c(summary(x$netbuy),sd(x$netbuy),length(x$netbuy))
  k[10,1:8]=c(summary(x$netsell),sd(x$netsell),length(x$netsell))
  k[11,1:8]=c(summary(x$size),sd(x$size),length(x$size))
  k[12,1:8]=c(summary(x$turn),sd(x$turn),length(x$turn))
  k[13,1:8]=c(summary(x$beta),sd(x$beta),length(x$beta))
  k[14,1:8]=c(summary(x$variance),sd(x$variance),length(x$variance))
  k=round(k,3)
  colnames(k)=c("min","25th","Median","Mean","75th","Max","Standard Deviation","N")
  row.names(k)=c("Return","Ownership concentration","Identity",
                 "Total institutions","Funds","QFII","Financial institutions",
                 "Other institutions","Netbuy","Netsell","Size","Turnover","Beta","Variance")
  return(k)
}