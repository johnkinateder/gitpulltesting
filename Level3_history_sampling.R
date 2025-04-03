library("ggplot2")

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
  print(history$Date[random.starting.month])
  ann.returns
}

Sim.years <- 35
N.years <- 5
d <- 1

for (lifetime in 1:100){
  ann.returns <- select_interval_of_annual_returns(N.years,monthly.returns)
  # Cycle through the selection of N.years consecutive historical annual returns until obtaining Sim.years
  # annual returns. 
  while(length(ann.returns)<Sim.years){
    ann.returns <- c(ann.returns,select_interval_of_annual_returns(N.years,monthly.returns))
  }
  
  equity_return <- ann.returns-1
  annual_spend_init <- 150000
  # equity_return <- rnorm(30,.07,.07)
  safe_return <- rnorm(Sim.years,.04,.01)
  inflation <- rnorm(Sim.years,.025,0.005)
  
  annual_spend <- rep(annual_spend_init,Sim.years)
  for (year in 2:Sim.years){
    annual_spend[year] <- annual_spend[year-1]*(1+inflation[year])
  }
  
  equity_index_max <- 1000
  
  equity_init <- 4000000
  safe_init <- 500000
  
  # Create vectors to contain the calculated values
  equity_index_year_beginning <- rep(1,Sim.years)
  e_bal_year_beginning <- rep(0,Sim.years)
  s_bal_year_beginning <- rep(0,Sim.years)
  e_bal_year_ending <- rep(0,Sim.years)
  s_bal_year_ending <- rep(0,Sim.years)
  dt <- 1:Sim.years
  
  equity_index_year_beginning[1] <- equity_index_max*(1+rnorm(1,.07,.07))
  if (equity_index_year_beginning[1]>0.95*equity_index_max){
    e_bal_year_beginning[1] <- equity_init-annual_spend[1]
    s_bal_year_beginning[1] <- safe_init
  } else {
    e_bal_year_beginning[1] <- equity_init
    s_bal_year_beginning[1] <- safe_init-annual_spend[1]
  }
  for (year in 1:(Sim.years-1)){
    e_bal_year_ending[year] <- e_bal_year_beginning[year]*(1+equity_return[year])
    s_bal_year_ending[year] <- s_bal_year_beginning[year]*(1+safe_return[year])
    equity_index_year_beginning[year+1] <- equity_index_year_beginning[year]*(1+equity_return[year])
    equity_index_max <- max(equity_index_max,equity_index_year_beginning[year+1])
    if (equity_index_year_beginning[year+1]>0.95*equity_index_max){
      e_bal_year_beginning[year+1] <- e_bal_year_ending[year]-annual_spend[year+1]
      s_bal_year_beginning[year+1] <- s_bal_year_ending[year]
    } else{
      e_bal_year_beginning[year+1] <- e_bal_year_ending[year]
      s_bal_year_beginning[year+1] <- s_bal_year_ending[year]-annual_spend[year+1]
    }
    print(year)
    print(annual_spend[year])
    print(e_bal_year_ending[year]+s_bal_year_ending[year])
  }
  tot_bal_begin <- e_bal_year_beginning+s_bal_year_beginning+annual_spend
  tot_bal_end <- e_bal_year_ending+s_bal_year_ending
  current.life <- data.frame(dt,inflation,equity_return,safe_return,annual_spend,equity_index_year_beginning, 
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
