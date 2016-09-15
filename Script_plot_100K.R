setwd("~/Python/DurakAI-master/09112016_Durak_v.4.2")

t <- read.csv('test_10in5.csv', header = FALSE);

card_names <- c(
  '6  clubs', '7 clubss', '8 clubs', '9 clubs', '10 clubs', 'J clubs', 'Q clubs', 'K clubs', 'A clubs',
  '6 diamonds', '7 diamonds', '8 diamonds', '9 diamonds', '10 diamonds', 'J diamonds', 'Q diamonds', 'K diamonds', 'A diamonds',
  '6  hearts', '7  hearts', '8  hearts', '9  hearts', '10  hearts', 'J  hearts', 'Q  hearts', 'K  hearts', 'A  hearts',
  '6 spades', '7 spades', '8 spades', '9 spades', '10 spades', 'J spades', 'Q spades', 'K spades', 'A spades'
)

pdf("Cross_test_10in5.pdf")
for (j in 1:9){
  
  p1 <- hist(t[,j],col=rgb(0,0,1,1/4),xlim=c(-20,20),ylim=c(0,25000),xlab='Change in Utility',ylab='Frequency', 
             main = paste("Histogram of" , card_names[j], "\n Mean = ", mean(t[,j]), "\n Std.Dev = ", sd(t[,j]) ));                     
  p2 <- hist(t[,j+9],col=rgb(1,0,0,1/4),xlim=c(-20,20),ylim=c(0,25000),xlab='Change in Utility',ylab='Frequency', 
             main = paste("Histogram of" , card_names[j+9], "\n Mean = ", mean(t[,j+9]), "\n Std.Dev = ", sd(t[,j+9]) ));
  p3 <- hist(t[,j+18],col=rgb(0,1,0,1/4),xlim=c(-20,20),ylim=c(0,25000),xlab='Change in Utility',ylab='Frequency', 
             main = paste("Histogram of" , card_names[j+18], "\n Mean = ", mean(t[,j+18]), "\n Std.Dev = ", sd(t[,j+18]) ));
  p4 <- hist(t[,j+27],col=rgb(0,1,1,1/4),xlim=c(-20,20),ylim=c(0,25000),xlab='Change in Utility',ylab='Frequency', 
             main = paste("Histogram of" , card_names[j+27], "\n Mean = ", mean(t[,j+27]), "\n Std.Dev = ", sd(t[,j+27]) ));
  plot( p1, col=rgb(0,0,1,1/4),xlab='Change in Utility',ylab='Frequency', 
        xlim=c(-20,20),ylim=c(0,25000), main=paste("Cross-Histograms of \n",card_names[j],',',card_names[j+9],',',card_names[j+18],',',card_names[j+27]))  # first histogram
  plot( p2, col=rgb(1,0,0,1/4), xlim=c(-20,20),ylim=c(0,25000),add=T)  # second
  plot( p3, col=rgb(0,1,0,1/4), xlim=c(-20,20),ylim=c(0,25000),add=T)
  plot( p4, col=rgb(1/4,0,0,1/4), xlim=c(-20,20),ylim=c(0,25000),add=T)
  
}
dev.off()
