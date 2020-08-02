library(tidyverse)

set.seed(1)

Npoints=10;
xi<-1:Npoints;
yobs<-xi + rnorm(Npoints);
yobs[seq(3,Npoints,3)]<-xi[seq(3,Npoints,3)]+rnorm(3,0,10);

df<-data.frame(xi=rep(1:10,7));
df$intercept<-rep(seq(-6,6,2),each=10);

df$yest<-df$xi+ df$intercept;
df$yobs<-rep(yobs,7);
df$squarederr<-round((df$yest-df$yobs)^2,2);
df$percenterr<-round(abs((df$yest-df$yobs)/df$yobs),2);


df2<-df %>% group_by(intercept) %>%
  summarise(meanSqerr=mean(squarederr),
            meanFractionerr=mean(percenterr));

df2$t<-paste0('yInt=',df2$intercept,' SqErr=',round(df2$meanSqerr,1),
             ' FrErr=',round(df2$meanFractionerr,1));
df2$xpos<-10;
df2$ypos<-df2$xpos + df2$intercept;
  
gg<-ggplot()+geom_point(data=df,aes(x=xi,y=yobs),color='red',size=3)+
  geom_line(data=df,aes(x=xi,y=yest,color=as.factor(intercept)),linetype="dashed",size=1.2)+
  xlim(0,15)+theme_classic()+xlab('x')+ylab('y')+
  theme(legend.position="none",text=element_text(size=15))+
  geom_text(data=df2,aes(x=xpos+2.3,y=ypos,label=t),size=5)
