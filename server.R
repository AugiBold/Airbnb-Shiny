library(shiny)


shinyServer(
  function(input, output, session){
    
    output$txtout0 = renderText({
      paste(
        "Airbnb, more than just a vacation rental company, was one of the market disruptors 
that started in 2008 which destabilized the hotel industry and changed the way people travel
by offering more options in booking places to stay.  
It got so popular that more and more people started using it as a business which eventually led to
certain cities to put in place regulations on short term rentals.

DATA: 
One of those cities is Washington, D.C and I obtained my data from insideairbnb.com, 
an independent non-commercial project that gathers publicly available Airbnb information 
with the express purpose of promoting awareness on this issue of Airbnb 
adversely affecting local neighborhoods and established real-estate businesses.  
The dataset only provided listings that were available for the last 12 months for Washington, D.C.  
The main dataset I will be working with is the listings data which includes fields like
listings, coordinate location, neighborhood, price per night, room type, number of reviews,
and availability days per year data on 39 neighborhoods that make up 8 Wards of Washington, D.C.

Image sources: https://en.wikipedia.org/wiki/Neighborhoods_in_Washington,_D.C.

OBJECTIVE:
With that in mind, through this project I wanted to explore 2 questions particularly
in the current post Covid state:
1) As a potential guest, where are the more affordable yet well rated and reviewed listings? 
2) As a potential host, which area has the most demand and likelihood of occupancy 
in the D.C market for listings?

APP BUILDER:
My name is Orkhon Boldbaatar (Augi) and I am a Data Science Fellow at NYC Data Science Academy 
with an Industrial Engineering background and operations management experience in 
manufacturing and consulting fields.  
I enjoy building community, appreciating performance arts, and traveling - which sparked
my interest to do this project.")
      
    }) 
    
    output$txtout1 = renderText({
      paste(
      "94% of all listings fall within $500 as shown in the distribution shown.
It is right skewed meaning that the very few expensive listings influence the distribution shape.
We can see from the plot that most of the listings is centered around $100 and is under $200.  
The median is $114 which is pretty affordable keeping in mind that this is the Capital city of the U.S.A.
Effects from COVID-19 likely caused the cost to be lower than what's considered normal 
in a metropolitan area.  You may change the price range of the listings using the slider on the left
to see the corresponding distribution.")
      }) 
    
    output$Price_Distribution = renderPlot({
      
      distPrice = input$Listing_Price_Range
      
      price_distribution = listings_shiny %>% filter(price < distPrice) %>% ggplot(aes(x = price)) +
        geom_histogram(aes(y = ..density..), color = "blue", fill = "darkseagreen3") +
        geom_density(alpha = .2, fill="chartreuse") + 
        xlab('Price ($)') + ylab('Density') + ggtitle('Price Distribution') + theme_bw()
      
      price_distribution
      
    })
    
    output$Room_type = renderPlot({
      
      room_type_count =listings_shiny %>% group_by(room_type) %>% summarise(count = sum(n())) %>% arrange(desc(count)) %>% 
        ggplot(aes(x=reorder(room_type, -count), y=count)) + 
        geom_bar(stat='identity', color = 'lightcoral', fill = 'lightcoral') + 
        ylab('# of Listings') + xlab('Room Type') + ggtitle('72% of Listings are Entire Home/Apt') + theme_bw() 
      
      room_type_count
      
    })
    
    output$Price_Density = renderPlot({
      
      price_density_room_type = listings_shiny %>% filter(price < 300) %>% 
        ggplot(aes(x=price, group = room_type, fill=room_type)) + 
        geom_density(adjust = 1.5, alpha =.4) + 
        xlab('Price ($)') + ylab('% of Total') + ggtitle('Price Density by Room Type') + theme_bw() 
      
      price_density_room_type
      
    })
    
    output$Ward_Avg = renderPlot({
      
      ward_avg_price = listings_shiny %>% group_by(ward) %>% summarise(avg_price = mean(price)) %>% arrange(avg_price)
      ward_avg_price %>%
        ggplot(aes(x=reorder(ward, avg_price), y=avg_price)) +
        geom_bar(stat='identity', color = 'palegreen3', fill = 'palegreen3')  +
        geom_text(aes(label= round(avg_price)), nudge_y=-20, color='black') +
        ylab('Avg price ($)') + xlab('Neighborhood Group') + ggtitle('Average Price by Ward (Neighborhood Group)') + theme_bw()
      
    })
    
    output$txtout2 = renderText({
      paste(
        "The closer you get to the Downtown area of DC (Ward 2) with the historic monuments and landmarks(Ward 2) 
along with affluent residential area(Ward 3) and government buildings(Ward 6) 
where most of the large events and public gatherings take place the pricier the listings get. 
On the other hand, Wards 5,7,8 are located furthest away from the Downtown area and 
can be shown to have the lowest average price per day from the graph above.")
      
    }) 
    
    output$Market_Size = renderPlot({
      
      market_size = sum(listings_shiny$availability_price)
      
      market_size_by_ward = listings_shiny %>% group_by(ward) %>% 
        summarise(ward_market_size = sum(availability_price), 
                  ward_ratio = round(ward_market_size/market_size*100)) %>% arrange(desc(ward_market_size))
      
      market_size_by_ward %>% ggplot(aes(fill = ward, y=ward_ratio, x='')) + 
        geom_bar(position="fill", stat="identity") +
        scale_y_continuous(labels=scales::percent) + 
        ylab('Ward %') + xlab('Market Size') + ggtitle('Market Size by Ward % - Top 3 wards make up 75% of Total Market!') + theme_bw() 
      
    })
    
    output$txtout3 = renderText({
      paste(
        "Interesting to see that the top 3 wards - Wards 2, 6, and 1 in the Market Size % stacked Barchart 
contribute 75% of the total Market size.  These same 3 wards are located closest to the Downtown area
of Washington D.C supporting the earlier trend seen in Average Price by Neighborhood Group Barplot.")
      
    }) 
    
    output$Reviews = renderPlot({
      
      total_reviews = sum(listings_shiny$number_of_reviews)
      reviews_by_ward = listings_shiny %>% group_by(ward) %>% 
        summarise(ward_reviews = sum(number_of_reviews), 
                  ward_review_ratio=round(ward_reviews/total_reviews*100)) %>% arrange(desc(ward_reviews))
      
      reviews_by_ward %>%
        ggplot(aes(x=reorder(ward, -ward_review_ratio), y=ward_review_ratio)) +
        geom_bar(stat='identity', color = 'cadetblue2', fill = 'cadetblue2')  + 
        geom_text(aes(label= paste0(ward_review_ratio, '%')), nudge_y= -1, color='black') +
        ylab('Review To Date %') + xlab('Neighborhood Group') + ggtitle('Reviews To Date % by Neighborhood Group') + theme_bw() 
      
    })
    
    output$txtout4 = renderText({
      paste(
        "The same top 3 wards(Wards 6,2,1) that made up 75% of the total Market Size
also account for 67% of All Reviews to date.
This makes sense as one would naturally expect most of the reviews to come from listings 
that make up 3/4ths of the Market.")
      
    }) 
    
    output$Reviews_ltm = renderPlot({
      
      total_reviews_ltm = sum(listings_shiny$number_of_reviews_ltm)
      
      reviews_ltm_by_ward = listings_shiny %>% group_by(ward) %>% 
        summarise(ward_reviews_ltm = sum(number_of_reviews_ltm), 
                  ward_review_ltm_ratio=round(ward_reviews_ltm/total_reviews_ltm*100)) %>% arrange(desc(ward_reviews_ltm))
      
      reviews_ltm_by_ward %>%
        ggplot(aes(x=reorder(ward, -ward_review_ltm_ratio), y=ward_review_ltm_ratio)) +
        geom_bar(stat='identity', color = 'deepskyblue', fill = 'deepskyblue')  +
        geom_text(aes(label= paste0(ward_review_ltm_ratio, '%')), nudge_y= -1, color='white') +
        ylab('Last 12 months Review %') + xlab('Neighborhood Group') +
        ggtitle('Last 12 months Review % by Neighborhood Group') + theme_bw() 
      
    })
    
    output$txtout5 = renderText({
      paste(
        "The last 12 months Review % shows that the most current review trends haven't changed much
from the Reviews To Date as the top 4 wards have stayed the same, 
contributing about the same % of the Reviews. 
Ward 5 has gone up a place by owning 2% more reviews in the last year compared to Ward 1 
so this is good to know from a potential host standpoint.")
      
    }) 
    
    
    output$txtout6 = renderText({
      paste(
        "CONCLUSION for Question #1 of analysis: 
Now that we have looked at the trends in Summary tab, 
the 1st question of interest can finally be answered: 
As a potential guest, Ward 1 and Ward 5 (as a second option) seem to offer the most affordable price 
on average (potential savings of $83 per day) while being considerably frequently reviewed and
at the same time located closest to the ward with the downtown area with all the historic landmarks.  
Ward 1 and Ward 5 also make up a sizeable portion of the Market size 
which means potential guests will have enough variety listings to choose from.

So use the select drop down menu on the right to see the Average Daily Price of Neighborhoods
in the Ward of your choice. 
(Ward 1 is shown as default since it is recommended as best choice for guests.)")
      
    }) 
    
    output$Neighborhood = renderPlot({
      
      ward_num = as.character(input$Ward) 
      
      listings_shiny %>% filter(ward == ward_num) %>% group_by(neighbourhood) %>%
        summarise(avg_price = mean(price)) %>% arrange(avg_price) %>%
        ggplot(aes(x=reorder(neighbourhood, -avg_price), y=avg_price)) +
        geom_bar(stat='identity', color = 'lightcoral', fill = 'lightcoral') +
        coord_flip() + geom_text(aes(label= round(avg_price)), nudge_y=-10, color='white') +
        ylab('Avg price ($)') + xlab('Neighborhood') + ggtitle('Average Listing Price by Neighborhood)') +
        theme_bw() + theme(text=element_text(size = 8))
      
    })
    
    output$txtout7 = renderText({
      paste(
        "Let's determine where the likelihood of a listing is to be more in demand 
by taking the ratio between the last 12 month Reviews and its availability by neighborhood-group(ward).  
Using the last 12 month review count as an indicator of likelihood of occupancy and 
the listing available days as a factor of meeting the market demand: ")
      
    }) 
    
    output$Likelihood = renderPlot({
      
      listings_shiny %>% group_by(ward) %>% 
        summarise(ward_reviews_ltm = sum(number_of_reviews_ltm), 
                  ward_availability = sum(availability), 
                  reviews_per_availability = ward_reviews_ltm/ward_availability) %>% 
        arrange(desc(reviews_per_availability)) %>%
        ggplot(aes(x=reorder(ward, -reviews_per_availability ), y=reviews_per_availability)) +
        geom_bar(stat='identity', color = 'deepskyblue', fill = 'deepskyblue') +
        ylab('Reviews per availability') + xlab('Neighborhood Group') +
        ggtitle('Likelihood of Demand by Neighborhood Group') + theme_bw() 
      
    })
    
    output$txtout8 = renderText({
      paste(
        "CONCLUSION for Question #2 of analysis:
Ward 5 and 1 would be the wards to look into for potential hosts to put up a listing
because they have the highest likelihood for demand currently and they are good picks for people 
who are looking for affordable options compared to Downtown area listings which are more expensive.  
Ward 7 isn't recommended because it only makes up 3% of the market size having less information 
to make an investment decision.")
      
    }) 
    
  }
  
  
)