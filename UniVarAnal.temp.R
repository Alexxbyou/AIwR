

CH1.demo<-readRDS("D:/bdtest/newtest/content/AI/2018-02-21-ai-analytics/CH1.demo.RDS")
CH1.out<-readRDS("D:/bdtest/newtest/content/AI/2018-02-21-ai-analytics/CH1.out.RDS")
pat.grp<-CH1.out[,1:2]
pat.grp$Death<-sapply(as.character(pat.grp$Death),function(x)switch(x,"0"="Alive","1"="Death"))
names(pat.grp)[2]<-"group"
CH1.demo<-left_join(CH1.demo,pat.grp)

data<-CH1.demo[,c("AGE","group")]


