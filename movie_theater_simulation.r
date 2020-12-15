# Cost for adults and children
ticket_cost <- 60
ticket_cost_child <- 30

# List 5 of your favorite movies
movies <- c('Toy Story', 'Another Round'='PG-13', 'Captain Phillips',
            'Once Upon a Time','The Assistant'='PG-13') 

# How many screens does the theater have? (assume 1 per movie)
screens <- 10

# How many seats does each theater hold
seats <- 100 

week_days <- rep(0, 7)  # Store totals for each day
theater_sim <- function(screen=screens,  # how many screens?
                        seat=seats,  # how many seats per screen?
                        ticket_costs=ticket_cost,  # adult ticket price
                        ticket_cost_childs=ticket_cost_child  # child ticket price
                        ){
  
# iterate through the week
  for (i in 1:length(week_days)) {
  
  # Keep track of total revenue for the day
    Total_revenue <- 0
  
  # iterate through the amount of screens on a particular day
  for (j in 1:length(screens)) {
    
    # Calculate  how many adults and children are watching the movie
    visitors_adults <- sample(seats, 1)
    visitors_children <- sample((seats - visitors_adults), 1)
    
    # Calculate the revenue for adults and children
    Total_revenue_adult = visitors_adults * ticket_cost 
    Total_revenue_children = visitors_children * ticket_cost_child
    
    # Calculate revenue, and add to running total for the day
    Total_revenue = Total_revenue_adult + Total_revenue_children + Total_revenue
    print(Total_revenue)
  }
  week_days[i] <- Total_revenue
}
print (week_days)
}

library(ggplot2)
# Create snacks
snacks <- c('Popcorn'= 9, 'Chocolate' = 17, 'Candy' = 15,
            'Chips' = 12, 'Cola'=13, 'almonds'=11)
snacks_children_types <- c('Popcorn'= 'Able', 'Chocolate'='Able', 'Candy'='Not-able',
                           'Chips' = 'Not-able', 'Cola'='Not-able', 'almonds'='Able')
snack = sample(c('Popcorn', 'Chocolate', 'Candy' ,'Chips' , 'Cola', 'almonds'),6)
snack
#for loop to limited number of customer instead of theater seats
for(i in 1:length(seats)){
  visitors_adults <- sample(seats, 1)
  visitors_children <- sample((seats - visitors_adults), 1)
  
  #Determining if the visitor bought a snack or not instead of the ticket price, If the visitor did not buy snack then the price will be the price of ticket only. If the visitor bought a snack then the price will be ticket price + snack price
  customer_without_snacks <- sample(ticket_cost, 1) 
  customer_with_snacks <- sample(((snacks + ticket_cost) - customer_without_snacks), 1) 
  
  # condition statement that determines whether a customer bought a snack or not
  if(customer_without_snacks < customer_with_snacks){
    customer_without_snacks = customer_without_snacks + 1
    cat('Numbers of customers who not bought snacks: ',customer_without_snacks,'\n') }
  customer_with_snacks = customer_with_snacks + 1
  cat('Numbers of customers who bought snacks: ',customer_with_snacks)
} 
#detemine the total purchases of customer who bought snack or not bought
total_snacks_purchases <- rbind(customer_without_snacks, customer_with_snacks)

cat('Numbers of customers who bought snacks or not: ', total_snacks_purchases)
cat('type of snack with numbers of customers who bought snacks or not respectively: ', snack," " ,total_snacks_purchases )


#determine the purcheses of each snack
total_purchese_according_snack <- rep(NA,length(customer_with_snacks))
total_purchese_according_snack[snack] <- sample(customer_with_snacks, 6,  replace=FALSE )
print(total_purchese_according_snack)
# Plot a results
barplot(total_purchese_according_snack,
        main = "The Total Purchases Of Each Snack-type ",
        xlab = "Snacks",
        ylab = "Total purchases",
        col = cm.colors(6))


# Condition decide if children should be able to buy snacks
snacks_children <- sample(snacks_children_types, 1)
if(snacks_children == 'Able'){
  print('Kids can buy snacks')
} else {
  print('Kids can not buy snacks')
}

# 3 Condition statement for movies that may be PG-13 and not appropriate for kids 
movies_rating <- sample(movies, 1)
if(movies_rating=='PG-13'){
  print('Not appropriate for kids')
} else {
  print('Appropriate for kids')
}

# simulate 2 movie theaters for 1 week
theater_01 <- theater_sim()
theater_02 <- theater_sim(seat=75, 
                          screen=3)
theater_03 <- theater_sim(ticket_costs =100,
                          ticket_cost_childs =60)

# make a barchart showing total revenue per day for each theater
barplot(theater_01,  
        main = "Theater #1 - Total Movie Revenue Per Day", 
        xlab = "Day of Week",  
        ylab = "Revenue (SAR)",  
        names.arg = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"),  # add day labels
        col = "pink",  
        density = 20,  
)

barplot(theater_02,  
        main = "Theater #2 - Total Movie Revenue Per Day",  # chart title
        xlab = "Day of Week",  
        ylab = "Revenue (SAR)",  
        names.arg = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"),  # add day labels
        col = "blue",  
        density = 20,  
)

barplot(theater_03,  
        main = "Theater #3 - Total Movie Revenue Per Day",  # chart title
        xlab = "Day of Week",  
        ylab = "Revenue (SAR)", 
        names.arg = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"),  # add day labels
        col = "purple",  
        density = 20,  
)

# (function) which day showing the highest rate of revenue
find_best_day <- function(theater){
  index_of_max_value <- which.max(theater)  # <- asks, which index has the max value?
  days_of_the_week <- c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
  best_day <- days_of_the_week[index_of_max_value]
  best_day
}

# share each theater's best day
paste('Theater #1 best day was', find_best_day(theater_01), 'with', paste0('SAR ', max(theater_01)))
paste('Theater #2 best day was', find_best_day(theater_02), 'with', paste0('SAR ', max(theater_02)))
paste('Theater #3 best day was', find_best_day(theater_03), 'with', paste0('SAR ', max(theater_03)))
