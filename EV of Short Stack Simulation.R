# Jason Laso

# This is my attempt to quantify the value of a 2.5 BB stack, 6 spots out of the money of a $570 tournament 
# at the Hard Rock in Hollywood, FL in November 2016. I was offered $100 for 25% equity in my tournament and 
# I accepted. My goal here is to try to estimate the EV of that deal by trying to simulate the tournament.

###################################################################################################



# Payouts

one = 92800
two = 53000
three = 33000
four = 27000
five=22000
six=18000
seven = 14000
eight = 11000
nine = 7700
finaltwo = 3500
top45 = 1735
min4 = 1372
min3= 1211
min2 = 1089
min = 1009
buyin=570

# Set number of simulations
iterations = 10000

# Initialize vectors
ev=c()
itm.odds=c()
ev.cash=c()
set.seed(1)

for(i in 1:iterations){
# Tournament simulation #
  
  # User inputs denoted "###". These are complete guesses as to frequencies in which I'd cash in certain
  # payout tiers.
    
    #Pct cashing  
    itm.odds[i] = rnorm(1, 22.5,3.75)/100    ###
    otm.odds = 1 - itm.odds[i]
    
    #PCt final table on ITM trials
    ft.odds = (rnorm(1, 7, 2)/100)*itm.odds[i] ###
    
    # On FT trials, twice as likely to finish top 3 and 1.5x middle 3 as you are to finish bottom 3
    ft.btm3.odds = ft.odds/4.5
    ft.mid3.odds = ft.odds/3
    ft.top3.odds = 2*ft.odds/4.5
    
    # Divide non-FT cashes into 100 shares
    non.ft.share = (itm.odds[i] - ft.odds)/100
    
    # Divide shares among payout tiers
    # Between 40-60% in the 4 min cash tiers
    min.odds = round(rnorm(1,18,3),0)   ###
    min2.odds = round(rnorm(1,12,3),0)  ###
    min3.odds = round(rnorm(1,10,3),0)  ###
    x=round(runif(1,40,60),0)
    min4.odds = ifelse(x - min.odds-min2.odds-min3.odds >0, x - min.odds-min2.odds-min3.odds, 0)
    
    # Finish between 19-45 (50% of remaining shares)
    top45.odds = round( ((100 - min.odds-min2.odds-min3.odds-min4.odds)*.5),0)
    
    # Finish 10-18 (whatever is left)
    finaltwo.odds = 100 - min.odds-min2.odds-min3.odds-min4.odds - top45.odds
    
    # calculate EV on positive and negative side
    ev.ft = ft.btm3.odds * ((seven+eight+nine)/3) + ft.mid3.odds * ((four+five+six)/3) + ft.top3.odds * ((one+two+three)/3) 
    ev.nonft = (non.ft.share * min.odds * min) + (non.ft.share * min2.odds * min2)+(non.ft.share * min3.odds * min3)+
      (non.ft.share * min4.odds * min4) + (non.ft.share * finaltwo.odds *finaltwo)+(non.ft.share * top45.odds * top45)
    ev.cash[i] = ev.ft + ev.nonft
    ev.noncash = otm.odds * -buyin
    
    #capture the EV of each trial
    ev = append(ev, ev.cash[i] + ev.noncash)
}

# Results for 25% equity of my expected tournament value
summary(ev*.25) #25% cash value

# Average cash for all trials in which I made the money
summary(ev.cash/itm.odds)  

# Graph of simulation
hist(ev*.25, breaks=30, main="EV of 25% at Time of Deal", xlab="EV")
abline(v = mean(ev*.25),
       col = "royalblue",
       lwd = 4)
abline(v = quantile(ev*.25,.25),
       col = "red",
       lwd = 2)
abline(v = quantile(ev*.25,.75),
       col = "red",
       lwd = 2)
text(x=mean(ev*.25), y=350, labels=paste("mean:",round(mean(ev*.25),2)), pos=4, col="blue")
text(x=quantile(ev*.25,.25), y=200, labels=paste("25th percentile:",round(quantile(ev*.25,.25),2)), pos=2, col="red")
text(x=quantile(ev*.25,.75), y=200, labels=paste("75th percentile:",round(quantile(ev*.25,.75),2)), pos=4, col="red")



# This says my EV at the time on average was about $130 for 25%. Given that I feel I was very "complimentary"
# of myself in some of my inputs in the simulation that may be slightly on the high side. Nevertheless, it
# likely indicates the $100 I accepted was probably not terribly far off my actual EV.

# I ended up cashing in 48th place for $1372, so I paid $343 back to the investor (plus the $100 he paid me).