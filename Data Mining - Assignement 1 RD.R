# Data Mining Assignement #1 
# Homework Team 2 Fall 2 
# Created on 10/7/2018 

#--------------------------------------------------ASSIGNMENT------------------------------------------------------

# A restaurant owner has a core menu that has remained constant throughout the past two years, while many
# other dishes have rotated on and off the menu. The owner has prepared a dataset of all orders containing
# elements of that core menu in which the customer also ordered wine. The dataset has 3 columns: orderNumber
# which identifies each individual person’s order at a table, order which itemizes the order’s components, and
# type which provides the type of item in the order (takes one of 3 values: ‘Meat’, ‘Side’, ‘Wine’).

# The restaurant owner would like to add wine suggestions to the menu next to each meat and analyze her
# customer ordering habits in general. She is also interested in showing a picture of a popular meal of hers in
# an ad in Gourmet Diner magazine. Your job is to address her interests and also to report anything else of
# interest in terms of customer ordering habits, all in a nice summary report that is complete but to the point.

#-------------------------------------------DATA CLEANING/TRANSFORMATION----------------------------------------------
# Import CSV file into R
df_restaurant = read.csv("restaurantData.csv")
summary(df_restaurant)
nrow(df_restaurant)

# There are a total of 26682 observations and 3 variables
# There are 8894 Meat types, 8894 Side types, and 8894 Wine types

# Make sure that the OrderNumber is sorted from least to greatest, convert OrderNumber to numeric
df_sorted = df_restaurant[order(df_restaurant$orderNumber),]
df_sorted$orderNumber = as.numeric(df_sorted$orderNumber)

#-------------------------------------------LOOKING AT ONLY MEAT AND WINE----------------------------------------
# Get rid of the side items for the pairing of the meat with the wine analysis portion of the report
no_side = df_sorted[!grepl("Side", df_sorted$type),]

# Convert dataframe into basket format with each order as a single row with the purchased 
# items seperated by columns. The function ddply() checks the ordernumber and pivots the order
# with same ordernumber in one line, separated by commas.

# Convert dataframe into transaction format
install.packages('plyr', dependencies= TRUE)
library('plyr')

# Convert dataframe into basket format with each order as a single row with the purchased 
# items seperated by columns. The function ddply() checks the ordernumber and pivots the order
# with same ordernumber in one line, separated by commas.
df_itemList_no_side = ddply(no_side,c("orderNumber"), 
                    function(df1)paste(df1$order, 
                                       collapse = ","))

# Now we can begin the Market Basket Analysis, since we have each transaction we no longer need the order number
df_itemList_no_side$orderNumber = NULL

#Rename column header for ease of use
colnames(df_itemList_no_side) = c("itemList")

# Write back to CSV to create a index value for each order (transaction IDs)
write.csv(df_itemList_no_side,"ItemList_no_side.csv", quote = FALSE, row.names = TRUE)

#-------------------------------------MARKET BASKET ANALYSIS / ASSOCIATION RULES-----------------------------------
# Now find the association rules
install.packages('arules', dependencies=TRUE)
library(arules)

# Read the CSV file that we created with the transaction IDs and each order by individual row
restaurant_no_side = read.transactions(file="ItemList_no_side.csv", rm.duplicates= TRUE, format='basket',sep=',',cols=1);
str(restaurant_no_side)
summary(restaurant_no_side)

# Here are the top 10 items (no sides) 
itemFrequencyPlot(restaurant_no_side, topN = 8)

# Create Market Basket Analysis Rules for meats and wines with NO sides
basket_analysis_no_side = apriori(restaurant_no_side,parameter = list(sup = 0.01, conf = 0.4,target='rules'))
inspect(basket_analysis_no_side)
summary(basket_analysis_no_side)

# Can also view basket_rules as a dataframe and use View()
df_basket_no_side <- as(basket_analysis_no_side,"data.frame")
View(df_basket_no_side)

#-------------------------------------------CONTINUE WITH MEAT, SIDE, and WINE-----------------------------------
# Convert dataframe into transaction format
install.packages('plyr', dependencies= TRUE)
library('plyr')

# Convert dataframe into basket format with each order as a single row with the purchased 
# items seperated by columns. The function ddply() checks the ordernumber and pivots the order
# with same ordernumber in one line, separated by commas.
df_itemList = ddply(df_sorted,c("orderNumber"), 
                     function(df1)paste(df1$order, 
                                        collapse = ","))

# Now we can begin the Market Basket Analysis, since we have each transaction we no longer need the order number
df_itemList$orderNumber = NULL

#Rename column header for ease of use
colnames(df_itemList) = c("itemList")

# Write back to CSV to create a index value for each order (transaction IDs)
write.csv(df_itemList,"ItemList.csv", quote = FALSE, row.names = TRUE)


#-------------------------------------MARKET BASKET ANALYSIS / ASSOCIATION RULES-----------------------------------
# Now find the association rules
install.packages('arules', dependencies=TRUE)
library(arules)

# Read the CSV file that we created with the transaction IDs and each order by individual row
restaurant = read.transactions(file="ItemList.csv", rm.duplicates= TRUE, format='basket',sep=',',cols=1);
str(restaurant)
summary(restaurant)

# Transactions are in quotes, which are unnecessary and result in some incorrect results
# restaurant@itemInfo$labels = gsub("\"","",restaurant@itemInfo$labels)

# Here are the top 10 items 
itemFrequencyPlot(restaurant, topN = 10)

# Create Market Basket Analysis Rules for meats and wines and sides
basket_analysis = apriori(restaurant,parameter = list(sup = 0.01, conf = 0.8,target='rules'))
inspect(basket_analysis)
summary(basket_analysis)

# Can also view basket_rules as a dataframe and use View()
df_basket <- as(basket_analysis,"data.frame")
summary(df_basket)
str(df_basket)

# Sort the rules by confidence 
basket_analysis_conf <- sort(basket_analysis, by='confidence', decreasing = TRUE)
summary(basket_analysis_conf)

# Sort the rules by Support 
basket_analysis_supp <- sort(basket_analysis, by='support', decreasing = TRUE)
summary(basket_analysis_supp)

# Now create a dataframe for the sorted confidence values for the rules 
df_basket_conf <- as(basket_analysis_conf,"data.frame")
View(df_basket_conf)
# Export dataframe to a csv file 
write.table(x = df_basket_conf, file = "/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Data Mining/Homework/market_basket.csv")

inspect(basket_analysis_conf[1:10])
# 95% of customers who ordered Cantina Pinot Bianco and Steamed Seasonal Veg also ordered the Pork Tenderloin
# 94% of customers who purchased the Cantina Pinot Bianco also ordered the Pork Tenderloin
inspect(basket_analysis_supp[1:10])
# The most popular meal is the Cantina Pinot Blanco ordered with the Pork Tenderlion
# The second most popular meal is the Roast Chicken with the Duckhorn Chardonnay
# The most popular FULL meal is the Cantina Pinot Bianco and Roasted Root Veg with the Pork Tenderloin
# The second most popular FULL meal is the Cantina Pinot Bianco and Steamed Seasonal Veg with the Pork Tenderloin 

# Check to see any interesting relationships in our restaurant orders 
library(arulesViz)
plot(basket_analysis)
plot(basket_analysis, method = "grouped", control = list(k = 5))
plot(basket_analysis, method="graph", control=list(type="items"))
plot(basket_analysis, method="paracoord",  control=list(alpha=.5, reorder=TRUE))

#-----------------------------------Plotting Frequency Tables for Meat/Sides/Wine----------------------------------
install.packages('plyr', dependencies= TRUE)
library('plyr')
install.packages('arules', dependencies=TRUE)
library(arules)
# MEAT
meat = df_sorted[!grepl("Side", df_sorted$type),]
meat1 = meat[!grepl("Wine", meat$type),]
meat2=meat1
meat2$order = factor(meat2$order, ordered = TRUE, levels = c("Pork Tenderloin", "Duck", "Roast Chicken", "Filet Mignon"))

#labels <- paste(c("Pork Tenderloin","Duck","Roast Chicken","Filet Mignon"))
x <- barplot(prop.table(table(meat2$order)),
        col = "Light Blue", main = "Angus Grange Steakhouse Meat Items", ylab = "Frequency", xlab = "Meat")
#text(x, par("usr")[3], labels = labels, srt = 40, adj = 1, xpd = TRUE)
#axis(2)

#SIDE
side = df_sorted[!grepl("Wine", df_sorted$type),]
side1 = side[!grepl("Meat", side$type),]
side2=side1
side2$order = factor(side2$order, ordered = TRUE, levels = c("Roasted Root Veg", "Steamed Seasonal Veg", "Risotto", 
                                                             "Baked Beans", "Mashed Potatoes", "Caesar Salad") )
y <- barplot(prop.table(table(side2$order)),
             col = "Beige", main = "Angus Grange Steakhouse Side Items", ylab = "Frequency", xlab = "Side", cex.names = .8)

#WINE
wine = df_sorted[!grepl("Meat", df_sorted$type),]
wine1 = wine[!grepl("Side", wine$type),]
wine2=wine1
wine2$order = factor(wine2$order, ordered = TRUE, levels = c("Cantina Pinot Bianco", "Duckhorn Chardonnay",
                                                             "Blackstone Merlot", "Meiomi Pinot Noir"))
z <- barplot(prop.table(table(wine2$order)),
             col = "Maroon", main = "Angus Grange Steakhouse Wine Items", ylab = "Frequency", xlab = "Wine")
