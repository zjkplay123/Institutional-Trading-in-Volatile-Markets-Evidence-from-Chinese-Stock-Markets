#full paper link: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3378571

#regression output in Table 5
Fun_Significance=function(x){
  m=mean(x)
  m=round(m,2)
  a=t.test(x,alternative=c("two.sided"))
  p=a$p.value
  if (p<=0.001){
    return(paste(m,"%","***",sep=""))
  } else if (p<=0.01 & p>0.001){
    return(paste(m,"%","**",sep=""))
  } else if (p<=0.05 & p>0.01) {
    return(paste(m,"%","*",sep=""))
  } else {
    return(paste(m,"%",sep=""))
  }
}

table_up_nost=function(x,y){
  option=c("NETBUY","netbuy2","netbuy3","wall","wfund","wQFII","wfinance","wother")
  new=x[option[y]][,1]
  x=data.frame(x,new=new)
  k=matrix(NA,nrow=31,ncol=9)
  name1=c("Upper","8~9.99%","6~7.99%","4~5.99%")
  name2=c()
  for (i in 1:4) {
    name2[i]=paste(name1[i],option[y],sep="*")
  }
  row.names(k)=c(name1[1],"",option[y],"",name2[1],"",name1[2],"",name2[2],"",name1[3],"",name2[3],"",
                 name1[4],"",name2[4],"","size","","turn","","variance","","beta","","constant",
                 "","Number","R^2","Adjust R^2")
  colnames(k)=c("Day 1","Day 2","Day 3","Day 4","Day 5","[6,10]",
                "[11,20]","[21,60]","[61,120]")
  dname=c("ab1","ab2","ab3","ab4","ab5","ab6_10","ab11_20","ab21_60","ab61_120")
  for (i in 1:9) {
    vab=x[dname[i]][,1]
    xx=data.frame(x,vab=vab)
    xx=subset(xx,!is.na(xx$vab))
    xx$vab=as.numeric(xx$vab)
    xx=subset(xx,!is.na(xx$new))
    xx$new=as.numeric(xx$new)
    xx=xx[c("vab","new","upper","eight","six","four","size","turn",
            "variance","beta","id","time")]
    p3=plm(vab~new+upper+eight+six+four+
             upper:new+eight:new+six:new+four:new+
             size+turn+variance+beta, data=xx, model="pooling",index=c("id","time")) 
    a=coeftest(p3,vcov=vcovHC(p3,type="HC0"))
    a=tidy(a)
    b=summary(p3)
    a=as.data.frame(a)
    a[2:5]=round(a[2:5],3)
    k[1,i]=add_star2(a[3,2],a[3,5])
    k[2,i]=add_bra(a[3,4])
    k[3,i]=add_star2(a[2,2],a[2,5])
    k[4,i]=add_bra(a[2,4])
    k[5,i]=add_star2(a[11,2],a[11,5])
    k[6,i]=add_bra(a[11,4])
    k[7,i]=add_star2(a[4,2],a[4,5])
    k[8,i]=add_bra(a[4,4])
    k[9,i]=add_star2(a[12,2],a[12,5])
    k[10,i]=add_bra(a[12,4])
    k[11,i]=add_star2(a[5,2],a[5,5])
    k[12,i]=add_bra(a[5,4])
    k[13,i]=add_star2(a[13,2],a[13,5])
    k[14,i]=add_bra(a[13,4])
    k[15,i]=add_star2(a[6,2],a[6,5])
    k[16,i]=add_bra(a[6,4])
    k[17,i]=add_star2(a[14,2],a[14,5])
    k[18,i]=add_bra(a[14,4])
    k[19,i]=add_star2(a[7,2],a[7,5])
    k[20,i]=add_bra(a[7,4])
    k[21,i]=add_star2(a[8,2],a[8,5])
    k[22,i]=add_bra(a[8,4])
    k[23,i]=add_star2(a[9,2],a[9,5])
    k[24,i]=add_bra(a[9,4])
    k[25,i]=add_star2(a[10,2],a[10,5])
    k[26,i]=add_bra(a[10,4])
    k[27,i]=add_star2(a[1,2],a[1,5])
    k[28,i]=add_bra(a[1,4])
    k[29,i]=nobs(b)
    k[30,i]=round(b$r.squared[1],3)
    k[31,i]=round(b$r.squared[2],3)
  }
  return(k)
}