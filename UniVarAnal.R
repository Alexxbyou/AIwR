##########################################################################
##########################################################################

# Title: AI Analysis - Visualization
# Description: Univariate visualization & Comparison


##########################################################################
##########################################################################

##########################
# Age processing
##########################

cutoff2label<-function(cutoff){
  np<-length(cutoff)
  result<-c(
    paste(cutoff[1:(np-2)],"-",cutoff[2:(np-1)]-1,sep=""),
    paste(cutoff[np-1],"+",sep="")
  )
  return(result)
}

age.cut<-function(age,cutoff=c(0,15+0:7*10)){
  cutoff<-c(0,cutoff,999)%>%
    unique()%>%
    sort()
  age.label<-cutoff2label(cutoff)
  
  cut(age,breaks=cutoff,labels=age.label)
}

##########################
# Age Visualization
##########################

coef.proc<-function(coef.df){
  
  return(coef.df)
}

##########################
# Univariate analysis
##########################

reg.anl<-function(
  data #data.frame(var,group)
){
  grp.col<-grep("grp|group",tolower(names(data)))
  data[,grp.col]<-as.factor(data[,grp.col])
  fml<-as.formula(paste(names(data)[2:1],collapse="~"))
  mdl<-glm(formula=fml,data=data,family="binomial")
  summary(mdl)$coef
}


##########################
# Visualization
##########################