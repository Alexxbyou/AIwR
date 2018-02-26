
##########################
# Get model structure
##########################
# input data should only have integer/numeric or factor as variable type
# input data should only have outcome and rest as predictors

ClassRef<-function(column){
  colclass<-class(column)
  ref<-ifelse(colclass=="factor",levels(column)[1],"")
  return(c(colclass,ref))
}

getClassRef<-function(data){
  classrefdf<-data.frame(t(sapply(data,ClassRef)))
  names(classrefdf)<-c("VarClass","Reference")
  return(classrefdf)
}




##########################
# Coefficient process function
##########################




##########################
# Knitr function
##########################

# model structure: 1.variable rename, 2. variable grouping
#mdl.strct<-data.frame(
#  Variable=names(mdl.data),
#  rename=c("Death","Age","Gender","Race","Marital","MediFund","DiabetesYear","PatType",names(mdl.data)[9:18],"MajorAmputation","MinorAmputation","BMI",names(mdl.data)[22:27]),
#  VarGrp=c("Outcome",rep("Demographics",4),rep("Medical History",3),rep("Comorbidity & Complication",12),rep("Physiological Measure",4),rep("Score",3)),
#  stringsAsFactors=F
#)

#mdl.strct<-cbind(mdl.strct,getClassRef(mdl.data))

#names(mdl.data)<-mdl.strct$rename

log.reg<-function(mdl.data){
  fml<-as.formula(paste(names(mdl.data)[1],"~."))
  mdl.glm<-glm(fml,family="binomial",data=mdl.data)
  mdl.glm$rsqrt<-mdl.glm
  return(mdl.glm)
}

coef.proc<-function(coef.df,conf=.95){
  normqtl<-qnorm(conf+(1-conf)/2)
  coef.df<-data.frame(
    Variable=row.names(coef.df),
    OddsRatio=exp(coef.df[,1]),
    Low=exp(coef.df[,1]-coef.df[,2]*normqtl),
    Up=exp(coef.df[,1]+coef.df[,2]*normqtl),
    Pvalue=coef.df[,4],
    Sig=pval.cut(coef.df[,4]),
    stringsAsFactors = F
  )
  row.names(coef.df)<-NULL
  names(coef.df)[3:4]<-paste(names(coef.df)[3:4],round(conf*100),sep="")
  coef.df<-coef.df[-1,]
  return(coef.df)
}



coef.intpt<-function(coef.df,mdl.data,mdl.strct){
  fct.row<-which(mdl.strct$VarClass=="factor")
  fct.list<-lapply(fct.row,function(r){
    var.org<-mdl.strct$rename[r]
    data.frame(
      Var.org=var.org,
      VarNCat=paste(mdl.strct$rename[r],as.character(unique(mdl.data[,var.org])),sep=""),
      Cat=as.character(unique(mdl.data[,var.org])),
      stringsAsFactors=F
    )
  })
  fct.lkup<-do.call(rbind.data.frame,fct.list)
  coef.df<-left_join(coef.df,fct.lkup,by=c("Variable"="VarNCat"))
  nfct.row<-is.na(coef.df$Var.org)
  coef.df$Var.org[nfct.row]<-coef.df$Variable[nfct.row]
  coef.df$Var.coef<-coef.df$Variable
  coef.df$Variable[!nfct.row]<-coef.df$Cat[!nfct.row]
  coef.df<-left_join(coef.df,mdl.strct[,c(2,3,5)],by=c("Var.org"="rename"))
  coef.df$color.cd<-as.numeric(as.factor(coef.df$VarGrp))
  return(coef.df)
}
coef.df.to.kable<-function(coef.df){
  coef.kable<-coef.df%>%
    mutate(
      Variable=cell_spec(Variable, bold=is.na(Cat))
    )%>%select(Variable,OddsRatio,Low95,Up95,Pvalue,Sig)%>%
    kable("html",escape = F,digits=c(0,2,2,2,4,0),align=c("l","r","r","r","r","r"),row.names = F)%>%
    kable_styling(c("striped","hover","condensed","responsive"),full_width=F)%>%
    row_spec(which(coef.df$Pvalue<.05),color = "#1E90FF")%>%
    row_spec(0,color = "white", background = "#1E90FF")
  #coef.kable
  # step 2: grouping for factors
  fct.var<-mdl.strct$rename[mdl.strct$VarClass=="factor"]
  if(length(fct.var)>0){
    for(v in fct.var){
      v.row<-which(coef.df$Var.org==v)
      coef.kable<-coef.kable%>%
        group_rows(v,v.row[1],tail(v.row,1))
    }
  }
  return(coef.kable) 
}



##########################
# Visualization
##########################

# Categorical variable visualization
reg.cat.vis<-function(var,coef.proc.df){
  coef.df<-coef.proc.df[coef.proc.df$Var.org==var,]
  coef.df$Variable<-factor(coef.df$Variable,levels=coef.df$Variable)
  gg<-ggplot(data=coef.df,aes(x=Variable,y=OddsRatio))+
    geom_bar(stat="identity",color="gray",width =.4,alpha=.8,fill="dodgerblue4")+
    geom_errorbar(aes(ymin=Low95, ymax=Up95),size=.5,colour="deeppink",width=.2)+
    geom_line(aes(x=1:nrow(coef.df),y=OddsRatio),colour="deeppink")+
    stat_smooth(aes(x = as.numeric(Variable), y = OddsRatio), method = "lm", se = FALSE,colour="#838B8B") +
    xlab(var)+ylab("Odds Ratio")+ggtitle(var)+
    theme(legend.position = "bottom",
          axis.ticks.x=element_blank(),
          axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(gg)
}

# Age effect visualization
reg.age.vis<-function(age.effect){
  names(age.effect)<-c("Age","Coefficient")
  coef.min<-min(predict(loess(Coefficient~Age,df,span = .9), age.effect$Age))
  df$Coefficient<-df$Coefficient-coef.min
  gg<-ggplot(df,aes(x=Age,y=Coefficient))+geom_point()+stat_smooth(span = 0.9)
  return(gg)
}

# 
coef.df<-coef.proc.df[coef.proc.df$VarGrp=="Comorbidity & Complication",]
coef.df<-coef.df[rev(order(coef.df$Variable)),]
reg.para.var.vis<-function(coef.df,title){
  coef.df$Variable<-factor(coef.df$Variable,levels=coef.df$Variable)
  coef.df$Effect<-"Negative"
  coef.df$Effect[coef.df$OddsRatio>1]<-"Positive"
  gg<-ggplot(data=coef.df)+
    geom_segment(aes(x=Variable,xend=Variable,y=1, yend=OddsRatio,color=Effect), size=8)+
    #geom_bar(mapping=aes(x=Variable,y=OddsRatio),stat="identity",fill="steelblue")+
    geom_point(aes(x=Variable,y=OddsRatio),size=1,colour="deeppink")+
    geom_errorbar(aes(x=Variable,ymin=Low95, ymax=Up95),size=.6,colour="deeppink",width=.4)+
    coord_flip()+ylab("Odds Ratio")+geom_hline(yintercept = 1,size=.8,color="Gray30")+
    ggtitle(title)+xlab("")+
    theme(legend.position = "bottom",
          axis.ticks=element_blank(),
          axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(gg)
}


