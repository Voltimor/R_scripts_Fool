

t <- read.csv('test_10in6.csv', header = FALSE);

card_names <- c(
  '6  clubs', '7 clubss', '8 clubs', '9 clubs', '10 clubs', 'J clubs', 'Q clubs', 'K clubs', 'A clubs',
  '6 diamonds', '7 diamonds', '8 diamonds', '9 diamonds', '10 diamonds', 'J diamonds', 'Q diamonds', 'K diamonds', 'A diamonds',
  '6  hearts', '7  hearts', '8  hearts', '9  hearts', '10  hearts', 'J  hearts', 'Q  hearts', 'K  hearts', 'A  hearts',
  '6 spades', '7 spades', '8 spades', '9 spades', '10 spades', 'J spades', 'Q spades', 'K spades', 'A spades')


p1 <- hist(t[,1],col=rgb(0,0,1,1/4),xlim=c(-40,40),ylim=c(0,250000),xlab='Change in Utility',ylab='Frequency', 
           main = paste("Histogram of" , card_names[1], "\n Mean = ", mean(t[,1]), "\n Std.Dev = ", sd(t[,1]) ))