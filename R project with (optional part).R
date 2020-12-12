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
highest_revenue_day = max(which.max(week_days))
cat("Revenue was the highest in the day:", highest_revenue_day)

week_day = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
barplot(week_days, names.arg = week_day, col = "lavender", border = "maroon",
        xlab = "Days", ylab = "Total Revenue", main = "The Week Revenue")
order(week_day, decreasing=TRUE)


# Optional Part
# 1 Create snacks (Popcorn, chocolate, candy, etc.) and associate a price 
# for each item created. Create a condition statement that determines whether 
# a customer bought a snack or not. You can even decide if children should be able to buy snacks.

library(ggplot2)
# Create snacks
snacks <- c('Popcorn'= 9, 'Chocolate' = 17, 'Candy' = 15,
            'Chips' = 12, 'Cola'=13, 'almonds'=11)
snacks_children_types <- c('Popcorn'= 'Able', 'Chocolate'='Able', 'Candy'='Not-able',
                           'Chips' = 'Not-able', 'Cola'='Not-able', 'almonds'='Able')
snack = c('Popcorn', 'Chocolate', 'Candy' ,'Chips' , 'Cola', 'almonds')

for(i in 1:length(seats)){
  visitors_adults <- sample(seats, 1)
  visitors_children <- sample((seats - visitors_adults), 1)
  customer_without_snacks <- sample(ticket_cost, 1)
  customer_with_snacks <- sample(((snacks + ticket_cost) - customer_without_snacks), 1)
  # condition statement that determines whether a customer bought a snack or not
  if(customer_without_snacks < customer_with_snacks){
    customer_without_snacks = customer_without_snacks + 1
    cat('Numbers of customers who not bought snacks: ',customer_without_snacks,'\n') }
  customer_with_snacks = customer_with_snacks + 1
  cat('Numbers of customers who bought snacks: ',customer_with_snacks)
} 
total_snacks_purchases <- rbind(customer_without_snacks, customer_with_snacks)

cat('Numbers of customers who bought snacks or not: ', total_snacks_purchases)

total_purchese_according_snack <- rep(NA,length(customer_with_snacks))
total_purchese_according_snack[snack] <- sample(customer_with_snacks, 6,  replace=FALSE )
print(total_purchese_according_snack)
# Plot a results
barplot(total_purchese_according_snack,
        main = "The number of purchases of each type of snack",
        xlab = "Snacks",
        ylab = "Total purchases",
        col = topo.colors(6))

# Condition decide if children should be able to buy snacks
snacks_children <- sample(snacks_children_types, 1)
if(snacks_children == 'Able'){
  print('Kids can buy snacks')
} else {
  print('Kids can not buy snacks')
}
# 2 Pretend you own multiple theaters and run two simulations to represent each theater
# How many screens does the theater have? (assume 1 per movie)
screens <- 10
# How many seats does each theater hold
seats <- 100

# Theater 1
theater1_screens <- sample(screens, 1)
theater1_seats <- sample(seats,1)
# Theater 2
theater2_screens <- sample(screens, 1)
theater2_seats <- sample(seats,1)

# Condition show the same number of screens or seating capacity per theater
if(theater1_screens != theater2_screens | theater1_seats != theater2_seats){
  cat('Theater 1 number of screens is: ', theater1_screens,'\n')
  cat('Theater 1 number of seats is: ', theater1_seats,'\n')
  cat('Theater 2 number of screens is: ', theater2_screens,'\n')
  cat('Theater 2 number of seats is: ', theater2_seats,'\n')
  theaters_screens <- rbind(theater1_screens,theater2_screens)
  theaters_seats <- rbind(theater1_seats,theater2_seats)
  
  cat('Theaters number of screens is: ', theaters_screens,'\n')
  cat('Theaters number of seats is: ', theaters_seats,'\n')
} else {
  print('We have the same numbers of screen and seats')
}
# plot the results
barplot(table(theaters_seats), table(theaters_screens), main="The theater and rate of seats",
        xlab="theaters seats",
        ylab="theaters screens",
        col = topo.colors(2))

# 3 Condition statement for movies that may be PG-13 and not appropriate for kids 
movies_rating <- sample(movies, 1)
if(movies_rating=='PG-13'){
  print('Not appropriate for kids')
} else {
  print('Appropriate for kids')
}