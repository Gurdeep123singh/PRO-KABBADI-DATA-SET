
# PROJECT BY GURDEEP SINGH

# importing player data into r studio
players=read.csv("PlayerData.csv",check.names = F,header=T,sep= ",",stringsAsFactors=F)
print(players)

scores=print(players$Points)

#barplot of scores

barplot(counts,main = "SCORES",xlab = "SCORES",col="yellow")

# mean of points

mean(players$Points)

#median of points

median(players$Points)

print(scores)

# barplot of frequency of match played

counts1=table(players$`Match Played`)
barplot(counts1,main = "frequency of match played",xlab = "MATCH PLAYED",col="green")
mean(players$`Match Played`)
median(players$`Match Played`)

# barplot of frequency of raids
counts=table(players$Raids)
barplot(counts,main = "FREQUENCY OF RAIDS" ,xlab = "RAIDS",col="blue")
mean(players$Raids)
median(players$Raids)

# barplot of frequency of tackles
counts2=table(players$Tackles)
barplot(counts,main = "FREQUENCY OF TACKLES" ,xlab = "TACKLES",col="red")
mean(players$Tackles)
median(players$Tackles)

#comparison between sucess and unsucess raids

plot(players$`Unsuccessful Raids`,type = "b",lwd=2,
     xaxt="n",ylim=c(0,300),col="orange",xlab=".....UNSUCCESS RAIDS......",
     ,ylab="....SUCCESS RAIDS....",main="...COMPARISON BETWEEN SUCCESS AND UNSUCESS RAIDS... ")
lines(players$`Successful Raids`,col="red",type="b",lwd=2)
lines(players$`Unsuccessful Raids`,col="orange",type="b",lwd=2)


legend("topright",legend = c("SUCCESS RAIDS","UNSUCCESS RAIDS"),lty=1
       ,lwd=2,pch = 21,col=c("red","orange"),ncol=2,bty = "n",cex=0.8,
       text.col =c("red","orange"),inset=0.01)
grid(nx=NA ,ny=8 ,lwd=1,lty=2,col="green")


#comparison between raids and tackles

plot(players$Raids,type = "b",lwd=2,
     xaxt="n",ylim=c(0,300),col="orange",xlab=" .........TACKLES..........",
     ,ylab="......RAIDS.....",main="..COMPARISON BETWEEN RAIDS AND TACKLES..")
lines(players$Tackles,col="red",type="b",lwd=2)
lines(players$Raids,col="orange",type="b",lwd=2)


legend("topright",legend = c("TACKLES","RAIDS"),lty=1
       ,lwd=2,pch = 21,col=c("orange","red"),ncol=2,bty = "n",cex=0.8,
       text.col =c("orange","red"),inset=0.01)
grid(nx=NA ,ny=8 ,lwd=1,lty=2,col="green")

#comparison between success and unsuccess tackles

plot(players$`Successful Tackles`,type = "b",lwd=2,
     xaxt="n",ylim=c(0,300),col="orange",xlab="......SUCCESS TACKLES......",
     ,ylab="....UNSUCESS TACKLES....",main="...COMPARISON BETWEEN SUCCESS AND UNSUCCESS RAIDS...")
lines(players$`Successful Tackles`,col="orange",type="b",lwd=2)
lines(players$`Unsuccessful Tackles`,col="black",type="b",lwd=2)

legend("topright",legend = c("SUCCESS TACKLES","UNSUCCESS TACKLES"),lty=1
       ,lwd=2,pch = 21,col=c("orange","black"),ncol=2,bty = "n",cex=0.8,
       text.col =c("orange","black"),inset=0.01)
grid(nx=NA ,ny=8 ,lwd=1,lty=2,col="green")


# no. of different types of roles of players

count3=table(players$Position)
count3
pie(count3,main="ROLES OF PLAYERS")
box()

  # different types of teams

count4=table(players$Team)
count4
pie(count4,main="team names")
box()    

 #Welch Two Sample t-test for raids by successful raids
 
 
  c=players$Raids
 d=players$`Successful Raids`
  t.test(c,d=NULL,mu=0,alternative=c("two.sided","less","greater"),conf.level=0.95,var.equal=F,paired=F)
 t.test(c,d)
 
 boxplot(c~d,xlab=" SUCCESSFUL RAIDS",ylab="RAIDS",col="green")
 
 #Welch Two Sample t-test for tackles by successful tackles
 
 a=players$Tackles
 b=players$`Successful Tackles`
 f=players$`Unsuccessful Tackles`
 
 t.test(a,b=NULL,mu=0,alternative=c("two.sided","less","greater"),conf.level=0.95,var.equal=F,paired=F)
 t.test(a,b)
 
 boxplot(a~b,ylab="TACKLES",xlab="SUCCESSFUL TACKLES",col="red")
 
# 1-sample proportions test without continuity correction(p test)
 
 # as  n=34   y=186  in future in kabbadi
 table(players$future)
 
 prop.test(34,34+186,p=.1,alternative=c("greater"),correct = "F")
 table(players$future)
 
 # prediction for raids and successful raids
 
 c=players$Raids
 d=players$`Successful Raids`
 e=players$`Unsuccessful Raids`
 # 1. scatterplot (ordering)
 plot(c,d,main="SCATTEROPLOT",xlab = "RAIDS",ylab = "SUCCESSFUL RAIDS",col="DARK BLUE")
 
 # 2.correlation
 cor(c,d)
 
 # 3.simple linear regression
 
 mod=lm(d~c)
 mod
 
 # 4.adding regression line
 #Y=Uy|x =bo+b1X
 abline(mod,col=2,lwd=1)
 summary(mod)
 
 
 # 5. names to acces regression objects
 
 attributes(mod)
 
 
 mod$coefficients
  confint(mod)
 
  # 6.analysis of variance table
  anova(mod)
  
  
  # 7.linear regression assumption
  plot(mod)
  
  # 7.1 4 graphs of asumptions
  par(mfrow=c(2,2))
  plot(mod)
  
  # prediction for tackles and successful tackles
  # 1. scatterplot (ordering)
  plot(a,b,main="SCATTEROPLOT",xlab = "TACKLES",ylab = "SUCCESSFUL TACKLES",col="RED")
  
  # 2.correlation
  cor(a,b)
  
  # 3.simple linear regression
  
  mod1=lm(b~a)
  mod1
  
  
  
  # 4.adding regression line
  #Y=Uy|x =bo+b1X
  abline(mod1,col=2,lwd=1)
  summary(mod1)
  
  # 5. names to acces regression objects
  
  attributes(mod1)
  
  mod$coefficients
  confint(mod1)
  
  # 6.analysis of variance table
  anova(mod1)
  
  # 7.linear regression assumption
  plot(mod1)
  
  # 7.1 4 graphs of asumptions
  par(mfrow=c(2,2))
  plot(mod1)
  
  # prediction for raids and unsuccessful raids
  # 1. scatterplot (ordering)
  plot(c,e,main="SCATTEROPLOT",xlab = "RAIDS",ylab = "UNSUCCESSFUL RAIDS",col="green")
  
  # 2.correlation
  cor(c,e)
  
  # 3.simple linear regression
  
  mod2=lm(e~c)
  mod2
  
  # 4.adding regression line
  #Y=Uy|x =bo+b1X
  abline(mod2,col=2,lwd=1)
  summary(mod2)
  

  
  # 5. names to acces regression objects
  
  attributes(mod2)
  
  mod2$coefficients
  confint(mod2)
  
  
  # 6.analysis of variance table
  anova(mod2)
  
  # 7.linear regression assumption
  plot(mod2)
  
  # 7.1 4 graphs of asumptions
  par(mfrow=c(2,2))
  plot(mod2)
  
  
  
  # scatter plots of successful and unsuccessful raids for having in single row
  
  par(mfrow=c(1,2))
  plot(c,d,col="red",lwd=2,xlab = "RAIDS",ylab = "SUCCESSFUL RAIDS")
  plot(c,e,col="yellow",lwd=2,xlab = "RAIDS",ylab = "UNSUCCESSFUL RAIDS")
   
  #scatter plots of successful and unsuccessful tackles for having in single row
  
  par(mfrow=c(1,2))
  plot(a,b,col="DARK BLUE",lwd=2,xlab = "TACKLES",ylab = "SUCCESSFUL TACLES")
  plot(a,f,col="DARK GREEN",lwd=2,xlab = "TACKLES",ylab = "UNSUCCESSFUL TACKLES")
  
  # scatterplot of tackles success rate 
  plot(players$`Tackle Success Rate`, main="SCATTEROPLOT",xlab = "TACKLES",ylab = "....TACKLES SUCCESS RATES...",col="green")  
  
  # barplot of career best points
  barplot(players$`Career Best Points`,main = "CAREER BEST POINTS",col = "purple")
  
  
  
  