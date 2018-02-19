age<-round(rnorm(1000,60,15))
age.n.grp<-data.frame(
  age=round(rnorm(2000,60,15)),
  group=sample(1:3,2000,replace=T)
)
age.n.grp$age[age.n.grp$group==2]<-age.n.grp$age[age.n.grp$group==2]+3
age.n.grp$age[age.n.grp$group==3]<-age.n.grp$age[age.n.grp$group==3]+6

prev.df<-data.frame(
  Disease=c("Diabetes","Hypertension","Dyslipidemia","Asthma","COPD","Heart Failure","CHD","Stroke","TIA","CKD","Hip Fracture","Oesteoprosis","Spine Fracture"),
  Prevalence=runif(13),
  Std=runif(13)
)

library(ggplot2)

# Create test data.
dat = data.frame(count=c(10, 60, 30), category=c("A", "B", "C"))

# Add addition columns, needed for drawing with geom_rect.
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))


p2 = ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect(colour="grey30") +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  labs(title="Customized ring plot")


ggplot() +
  geom_bar(data=data, mapping=aes(x=3,y=Perc,fill=Category),stat="identity",colour="grey30") +
  coord_polar(theta="y") +
  xlab("")+ylab("")+ggtitle(Title)
  xlim(c(0, 4)) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
