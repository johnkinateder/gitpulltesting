library("ggplot2")
set.seed(1)
# Read a csv file with a history of monthly asset values from latest to oldest, and calculate the monthly.returns
history <- read.csv("Downloads/s_and_p_equal_weight.csv")
history <- history[nrow(history):1,]
monthly.returns <- history$Price[-1]/history$Price[-length(history$Price)]

# function to construct a sequence of annual returns of length N.years from an actual interval in history present in 
# monthly.returns
select_interval_of_annual_returns <- function(N.years,monthly.returns){
  # first pick a random starting date at least N.years prior to end of monthly.returns dataset
  N.months <- length(monthly.returns)
  last.eligible.starting.month <- N.months-N.years*12+1
  # then calculate the annual returns by multiplying the 12 monthly returns in each year starting with the starting date
  ann.returns <- 1:N.years
  # Generate index of random starting month
  random.starting.month <- round(runif(1,1,last.eligible.starting.month)) 
  start.index <- random.starting.month
  for (i in 1:N.years){
    end.index <- start.index+11
    ann.returns[i] <- prod(monthly.returns[seq.int(start.index,end.index)])
    start.index <- end.index+1
  }
#  print(history$Date[random.starting.month])
  ann.returns
}

Sim.years <- 35
N.years <- 5
d <- 1
equity_init <- 4000000
safe_init <- 500000
annual_spend_init <- 150000

for (lifetime in 1:1000){
  print(lifetime)
  ann.returns <- select_interval_of_annual_returns(N.years,monthly.returns)
  # Cycle through the selection of N.years consecutive historical annual returns until obtaining Sim.years
  # annual returns. 
  while(length(ann.returns)<Sim.years){
    ann.returns <- c(ann.returns,select_interval_of_annual_returns(N.years,monthly.returns))
  }
  
  equity_return <- ann.returns-1
  
  last.working.equity.return <- select_interval_of_annual_returns(N.years,monthly.returns)[1] -1
  # equity_return <- rnorm(30,.07,.07) # Alternative Equity Return Model
  safe_return <- rnorm(Sim.years,.04,.01)

  # PRIMARY INITIALIZATION PARAMETERS  
  
  equity_index_max <- 1000
  
  # Create vectors to contain the calculated values
  equity_index_year_beginning <- rep(1,Sim.years)
  e_bal_year_beginning <- rep(0,Sim.years)
  s_bal_year_beginning <- rep(0,Sim.years)
  e_bal_year_ending <- rep(0,Sim.years)
  s_bal_year_ending <- rep(0,Sim.years)
  dt <- 1:Sim.years
  
  # Initialize the first year index beginning with a random return
  equity_index_year_beginning[1] <- equity_index_max*(1+last.working.equity.return)
  e_bal_year_beginning[1] <- equity_init
  s_bal_year_beginning[1] <- safe_init
  for (year in 1:(Sim.years-1)){
    annual_spend <- rep(annual_spend_init,(Sim.years+4))
    inflation <- rnorm(Sim.years+4,.025,0.005)
    for (i in 2:(Sim.years+4)){
      annual_spend[i] <- annual_spend[i-1]*(1+inflation[i])
    }
    inflation <- inflation[1:Sim.years]
    four.years.cash <- sum(annual_spend[year+0:3]) # Take sum of current year spend plus next 3
    half.safe.deficit <- 0.5*max(0,four.years.cash - s_bal_year_beginning[year])
    if (equity_index_year_beginning[year]>0.95*equity_index_max){
     # print("Up year for stocks; taking annual spend from equity plus half safe deficit")
      e_bal_year_beginning[year] <- e_bal_year_beginning[year]-annual_spend[year]-half.safe.deficit
      s_bal_year_beginning[year] <- s_bal_year_beginning[year]+half.safe.deficit
    }
    else{
      #  print("Down year for stocks")
        if (s_bal_year_beginning[year]>annual_spend[year]){
      #       print("Sufficient safe funds to cover annual expenses, taking cash from safe pile")
             s_bal_year_beginning[year] <- s_bal_year_beginning[year]-annual_spend[year]
        } 
        else{
      #      print("Insufficient safe funds to cover annual expenses, depleting safe pile and taking some from equities")
            e_bal_year_beginning[year] <- e_bal_year_beginning[year]- annual_spend[year]-s_bal_year_beginning[year]
            s_bal_year_beginning[year] <- 0
        }
    }
    e_bal_year_ending[year] <- e_bal_year_beginning[year]*(1+equity_return[year])
    s_bal_year_ending[year] <- s_bal_year_beginning[year]*(1+safe_return[year])
    e_bal_year_beginning[year+1] <- e_bal_year_ending[year]
    s_bal_year_beginning[year+1] <- s_bal_year_ending[year]
    equity_index_year_beginning[year+1] <- equity_index_year_beginning[year]*(1+equity_return[year])
    equity_index_max <- max(equity_index_max,equity_index_year_beginning[year+1])
  }
  ann.spend <- annual_spend[1:Sim.years]
  tot_bal_begin <- e_bal_year_beginning+s_bal_year_beginning+ann.spend
  tot_bal_end <- e_bal_year_ending+s_bal_year_ending
  current.life <- data.frame(dt,inflation,equity_return,safe_return,ann.spend,equity_index_year_beginning, 
                  e_bal_year_beginning,s_bal_year_beginning,
                  tot_bal_begin,e_bal_year_ending,s_bal_year_ending,tot_bal_end,lifetime)  
  if (is.data.frame(d)) d <- rbind(d,current.life)
  else d <- current.life
}

ggplot(d, aes(x=dt, y=tot_bal_end, color=lifetime))+geom_point(size=1)+
  scale_y_continuous(limits=c(-2000000,20000000),labels = scales::dollar_format())+
  labs(x="Years into Retirement",y="Total Balance",title="Applying Level3 Strategy",subtitle="Dollar Balance Over Time")
# scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K"))

year=10
plot_ecdf <- function(year){
  dist <- d[d$dt==year,]
  left_censored <- lapply(dist$tot_bal_end, function(x) max(x,-5000000))
  dist$boundedbalance <- as.numeric(lapply(left_censored, function(x) min(x,30000000)))
  ggplot(dist,aes(boundedbalance)) + stat_ecdf(geom = "step")+
    labs(subtitle="Applying Level3 Strategy",
         y = "F(Balance)", x="Dollar Balance")+
    scale_x_continuous(limits=c(-5000000,30000000),breaks = scales::pretty_breaks(n = 10), labels = scales::dollar_format(scale = .000001, suffix = "M"))+
    theme_classic()+
    geom_hline(yintercept = c(0.02,0.05))+
    geom_vline(xintercept=0)+
    ggtitle( paste( "Empirical Cumulative Distribution of Balance after", year, "years") )
}
plot_ecdf(5)
plot_ecdf(10)
plot_ecdf(20)
plot_ecdf(30)
plot_ecdf(34)

