setwd("~/Python/DurakAI-master/09112016_Durak_v.4.2/R_scripts")

t <- read.csv('Trumps_1M.csv', header = FALSE);

N<-1000000
trumps<-matrix(0, nrow=4, ncol=36)
trumps_cond<-matrix(0,nrow=N, ncol=36)
card_names <- c(
  '6 clubs', '7 clubs', '8 clubs', '9 clubs', '10 clubs', 'J clubs', 'Q clubs', 'K clubs', 'A clubs',
  '6 diamonds', '7 diamonds', '8 diamonds', '9 diamonds', '10 diamonds', 'J diamonds', 'Q diamonds', 'K diamonds', 'A diamonds',
  '6 hearts', '7 hearts', '8 hearts', '9 hearts', '10 hearts', 'J hearts', 'Q hearts', 'K hearts', 'A hearts',
  '6 spades', '7 spades', '8 spades', '9 spades', '10 spades', 'J spades', 'Q spades', 'K spades', 'A spades'
)

suits <- c(rep('C',9),rep('D',9),rep('H',9),rep('S',9))

for (j in 1:36)
  { 
    C<-0; D<-0; H<-0; S<-0;
    for (i in 1:N)
    {
      z <- substr(t[i,j],2,2);
      ifelse(z=="C",C<-C+1,ifelse(z=="D",D<-D+1,ifelse(z=="H",H<-H+1,S<-S+1)))
    }
    trumps[1,j]<-C/N;
    trumps[2,j]<-D/N;
    trumps[3,j]<-H/N;
    trumps[4,j]<-S/N;
  }


pdf("Trumps_distribution_1M.pdf")
for (j in 1:36)
  {
barplot(trumps[,j], col=rgb(0,0,1,1/4),ylim=c(0,1),main=paste("Distribution for",card_names[j]), horiz=FALSE,
        names.arg=c("Clubs", "Diamonds", "Hearts","Spades"))
}
dev.off()

pdf("Trumps_distribution_1M_line.pdf")
for (i in 1:4)
{
  plot(trumps[i,], type='l',col=rgb(0,0,1,1/4),ylim=c(0,1),main=paste("Distribution for",suits[9*i],"\n Mean = ", mean(trumps[i,])))
}
dev.off()