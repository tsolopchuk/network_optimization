########################CLEANING#################################
library(readr)
#load monthly data and create annual data frame 
data_Jan2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/Jan_2019.csv")
data_Feb2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/Feb_2019.csv")
data_Mar2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/Mar_2019.csv")
data_Apr2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/Apr_2019.csv")
data_May2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/May_2019.csv")
data_Jun2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/Jun_2019.csv")
data_Jul2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/Jul_2019.csv")
data_Aug2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/Aug_2019.csv")
data_Sep2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/Sep_2019.csv")
data_Oct2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/Oct_2019.csv")
data_Nov2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/Nov_2019.csv")
data_Dec2019 <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/Annual Data/Dec_2019.csv")

data_flights <- rbind.data.frame(data_Jan2019, data_Feb2019, data_Mar2019, data_Apr2019, data_May2019, data_Jun2019,
                                 data_Jul2019, data_Aug2019,data_Sep2019, data_Oct2019, data_Nov2019, data_Dec2019)

library(dplyr) 
#filter on United airlines 
data_fligths_UA <- data_flights %>%
  filter(OP_UNIQUE_CARRIER == "UA")

data_fligths_UA <- data_fligths_UA[c(1,2,3,4,5,8)]

library(lubridate)
library(stringr)

#create weekday, month, day columns 
data_fligths_UA <- data_fligths_UA %>% mutate(wday=wday(FL_DATE), day=day(FL_DATE), month=month(FL_DATE)) 
data_fligths_UA$wday <- str_replace_all(as.factor(data_fligths_UA$wday), c("1"="Sun" , "2"="Mon" , "3"="Tue", 
                                                                            "4"= "Wed", "5" ="Thu" , "6"= "Fri",
                                                                            "7"="Sat" ))
data_fligths_UA$wday <-factor(data_fligths_UA$wday, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))


#load data set, use join function to convert Airport IDs to three letter codes
L_AIRPORT <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/L_AIRPORT.csv")
L_AIRPORT_ID <- read_csv("C:/Users/mattn/Desktop/DAP/Cleaning and Network/L_AIRPORT_ID.csv")

#set origin codes and descriptions
data_fligths_UA <- left_join(data_fligths_UA, L_AIRPORT_ID, by=c("ORIGIN_AIRPORT_ID" ="Code"))
data_fligths_UA <- left_join(data_fligths_UA, L_AIRPORT, by = c("Description" = "Description"))

colnames(data_fligths_UA)[11] = "Origin_Code"
colnames(data_fligths_UA)[10] = "Origin_Description"

#set destination codes and descriptions
data_fligths_UA <- left_join(data_fligths_UA, L_AIRPORT_ID, by=c("DEST_AIRPORT_ID" ="Code"))
data_fligths_UA <- left_join(data_fligths_UA, L_AIRPORT, by = c("Description" = "Description"))
View(head(data_fligths_UA))

colnames(data_fligths_UA)[13] = "Destination_Code"
colnames(data_fligths_UA)[12] = "Destination_Description"

View(head(data_fligths_UA[-c(5,6)]))

#create O_D pairs and ad them to the dataset 
O_D_pairs <- as.character(paste(data_fligths_UA$Origin_Code, "-", data_fligths_UA$Destination_Code))
data_fligths_UA <- as.data.frame(cbind(data_fligths_UA, O_D_pairs))

data_fligths_UA <- data_fligths_UA[c(1,2,3,4, 7,8,9,10,11,12,13,17)] #remove extra vars

################CH1: TOTAL PER MONTH ####################
library(ggplot2)
library(ggthemes)

#plot total monthly flights 
daily_flights2 <- data_fligths_UA %>%
  group_by(month) %>%
  count(month)

head(daily_flights2)

ggplot(daily_flights2, aes(x=as.factor(month), y=n)) + 
  geom_path(group=1, lwd=1, color="dark green") +
  scale_y_continuous("Number of Flights per Month", limits = c(40000, 65000)) +
  labs(title = "United Airlines - Total Flights per Month", subtitle = "Average of 56562 flights per month",
       x= "Month") +
  geom_hline(yintercept = 56562, lwd=1, color ="red", linetype=1) +
  theme_gdocs() +
  geom_point(size=3, color="dark green") 

mean(daily_flights2$n)

################CH2: BY MONTH####################
library(viridis)
#plot monthly flights by day 

daily_flights3 <- data_fligths_UA %>%
  group_by(month, day) %>%
  count(month, day)

tmp1 <- daily_flights3 %>%
  mutate(month2=month)

tmp1 <- as.data.frame(tmp1)
tmp1 <- tmp1[1:4]

tmp1 %>%
  ggplot( aes(x=day, y=n)) +
  geom_line( data=tmp1 %>% dplyr::select(-month), aes(group=month2), color="grey", size=0.5, alpha=0.5) +
  geom_line( aes(color=month), color="#69b3a2", size=1.2 )+
  scale_color_viridis(discrete = TRUE) +
  theme_gdocs() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  labs(x="Day of the Month", y="Number of Daily Flights") +
  ggtitle("United Airlines - Daily Flights by Month") +
  facet_wrap(~month)

################CH3: BY WEEKDAY ####################

#plot daily flights by weekday 
daily_flight3 <- data_fligths_UA %>% 
  group_by(FL_DATE, wday) %>%
  count(FL_DATE, wday)
head(daily_flight3)
  
ggplot(daily_flight3, aes(x=FL_DATE, y=n, color=as.factor(wday))) +
  geom_point(size=1, alpha=0.1 , color="black") +
  geom_path(group=1, lwd=1.2)+
  facet_grid(wday~.) +
  geom_hline(yintercept = 1860, lwd=1, color ="red", linetype=1) +
  theme_gdocs() + 
  labs(title="Number of Daily Flights by Weekday", x="Date", y="Number of Daily Fights", color="Weekday",
       subtitle="Average of 1860 flights per day") +
  scale_y_continuous(breaks = c(1300,1500,1700,1900))

mean(daily_flight3$n)

#################CH4: MOST POP ROUTES######################
#most popular destinations
average_monthly <- data_fligths_UA %>%
  group_by(month, O_D_pairs) %>%
  count(month, O_D_pairs) %>%
  arrange(desc(n)) %>%
  group_by(O_D_pairs) %>%
  summarise(avg = mean(n))%>%
  arrange(desc(avg)) %>%
  filter(avg >300) %>%
  arrange(desc(avg))

head(average_monthly)

average_monthly %>%
ggplot(aes(x=O_D_pairs, y=avg) ) +
  geom_segment(aes(x=O_D_pairs ,xend=O_D_pairs, y=0, yend=avg), color="#69b3a2", lwd=2) +
  geom_point(size=5, color="dark green") +
  coord_flip() +
  theme_gdocs() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("Origin-Destination Pairs") +
  ylab("Average Monthly Flights") +
  labs(title = "Most Popular Routes - United Airlines", subtitle= "Over 300 flights per month") +
  scale_y_continuous(limits=c(0, 450))


################CH5: INBOUND/OUTBOUND HUBS####################
#let's find inbound and outbound flights from different hubs 

inbound <- data_fligths_UA %>%
  filter(Destination_Code %in% c("ORD" , "EWR", "IAH", "SFO", "DEN", "LAX" ,"IAD")) %>%
  group_by(month, Destination_Code) %>%
  count(month, Destination_Code) %>%
  group_by(Destination_Code) %>%
  summarise(avg_inbound=mean(n))

outbound <- data_fligths_UA %>%
  filter(Origin_Code %in% c("ORD" , "EWR", "IAH", "SFO", "DEN", "LAX" ,"IAD")) %>%
  group_by(month, Origin_Code) %>%
  count(month, Origin_Code) %>%
  group_by(Origin_Code) %>%
  summarise(avg_outbound=mean(n))

total_flight_by_HUB <- left_join(inbound, outbound, by =c("Destination_Code" = "Origin_Code"))

total_flights <- total_flight_by_HUB %>%
  mutate (total = avg_inbound+ avg_outbound) %>%
  arrange(desc(total))

head(total_flights)

library(tidyr)
total_flight_by_HUB2 <- gather(total_flight_by_HUB, "flights", "value", 2:3)
head(total_flight_by_HUB2)

ggplot(total_flight_by_HUB2, aes(x= reorder(Destination_Code, -value), y=value, fill=as.factor(flights))) +
  geom_bar(stat="identity",position="stack") +
  theme_gdocs() + 
  labs(title="Inbound & Outbound Flights - United Airlines US HUBs",
       x = "Hubs", y="Total Flights")

################CH6: CONNECTIVITY MAP####################
#now let's see how the hubs are connected to the spokes 
airports <- read_csv("airports_coordinates.csv")
flights <- read_csv("NUMofFlights.csv")
library(maps)
library(geosphere)

pal <- colorRampPalette(c("#6e6060", "white", "#9c0c0c"))
colors <- pal(100)

map("world", col="#97a8b8", fill=TRUE, bg="#041f36", lwd=0.05, xlim=xlim, ylim=ylim)

fsub <- flights[flights$airline == "UA",]

fsub <- fsub[order(fsub$cnt),]
maxcnt <- max(fsub$cnt)
for (j in 1:length(fsub$airline)) {
  air1 <- airports[airports$iata == fsub[j,]$airport1,]
  air2 <- airports[airports$iata == fsub[j,]$airport2,]
  
  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE)
  colindex <- round( (fsub[j,]$cnt / maxcnt) * length(colors) )
  lines(inter, col=colors[colindex], lwd=1.5)
}   

################CH7: IGRAPHS####################

library(igraph)
library(dplyr)
connections_matrix <- as.matrix(flights[c(2,3)])
g <- graph_from_data_frame(connections_matrix, directed=TRUE)
plot(g,
     vertex.size=2,
     edge.arrow.size = 0.05,
     layout = layout.grid)

?layout

#Airport with the highest degree
V(g)$name[degree(g)==max(degree(g))]

#Airport with the highest closenses centrality
V(g)$name[closeness(g)==max(closeness(g))]

#Airport with the highest degree centrality
V(g)$name[degree(g) ==max(degree(g))]

#Centralization of the graph (based on degree)
centr_degree(g, mode="all", loops = TRUE)

#closeness & degree centrality 
network_closeness <- closeness(g, V(g))
View((network_closeness))

degree_centrality <- degree(g, V(g))
View(degree_centrality)

?centr_degree
###############CH8: DISTANCE####################
library(geosphere)
HUBs_loc <- airports %>%
  filter(iata %in% c("ORD", "EWR", "IAH", "SFO", "DEN"))
head(HUBs_loc)

library(dplyr)
ORD_dist <- distGeo(airports[, c("long","lat")], HUBs_loc[4,c("long", "lat")]) / 1609.35
distance <- cbind(ORD_dist, airports)

EWR_dist <- distGeo(airports[, c("long","lat")], HUBs_loc[2,c("long", "lat")]) / 1609.35
distance <- cbind(EWR_dist, distance)

DEN_dist <- distGeo(airports[, c("long","lat")], HUBs_loc[1,c("long", "lat")]) / 1609.35
distance <- cbind(DEN_dist, distance)

SFO_dist <- distGeo(airports[, c("long","lat")], HUBs_loc[5,c("long", "lat")]) / 1609.35
distance <- cbind(SFO_dist, distance)

IAH_dist <- distGeo(airports[, c("long","lat")], HUBs_loc[3,c("long", "lat")]) / 1609.35
distance <- cbind(IAH_dist, distance)

distance <- distance %>%
  mutate(min = pmin(IAH_dist, SFO_dist, ORD_dist, EWR_dist, DEN_dist))

head(distance)

distance2 <- gather(distance, key= "HUB", value = "distance",1:5 )
head(distance2)
View((distance))

HUB_ass <- as.character(ifelse(distance2$distance == distance2$min, distance2$HUB, 0))
distance2 <- cbind(HUB_ass, distance2)

distance3 <- distance2 %>% 
  filter(HUB_ass %in% c("IAH_dist", "EWR_dist", "SFO_dist", "DEN_dist", "ORD_dist"))

View((distance3))
head(distance3)
assignment <- left_join(flights, distance3, by = c("airport2"= "iata"))

assignment_only <- assignment [c(3,6)]
#EWR
assignment_EWR <- assignment_only %>% 
  filter(HUB_ass == "EWR_dist") %>% distinct()
View(assignment_EWR)

connections_matrix_EWR <- as.matrix(assignment_EWR)
View(connections_matrix_EWR)
g <- graph_from_data_frame(connections_matrix_EWR, directed=FALSE)
plot(g,
     vertex.size=2,
     edge.arrow.size = 0.05,
     layout = layout_nicely,
     main ="EWR distribution")

#ORD
assignment_ORD <- assignment_only %>% 
  filter(HUB_ass == "ORD_dist") %>% distinct()

connections_matrix_ORD <- as.matrix(assignment_ORD)
g <- graph_from_data_frame(connections_matrix_ORD, directed=FALSE)
plot(g,
     vertex.size=2,
     edge.arrow.size = 0.05,
     layout = layout_nicely,
     main ="ORD distribution")

#IAH
assignment_IAH <- assignment_only %>% 
  filter(HUB_ass == "IAH_dist") %>% distinct()

connections_matrix_IAH <- as.matrix(assignment_IAH)
g <- graph_from_data_frame(connections_matrix_IAH, directed=FALSE)
plot(g,
     vertex.size=2,
     edge.arrow.size = 0.05,
     layout = layout_nicely,
     main ="IAH distribution")

#DEN
assignment_DEN <- assignment_only %>% 
  filter(HUB_ass == "DEN_dist") %>% distinct()

connections_matrix_DEN <- as.matrix(assignment_DEN)
g <- graph_from_data_frame(connections_matrix_DEN, directed=FALSE)
plot(g,
     vertex.size=2,
     edge.arrow.size = 0.05,
     layout = layout_nicely,
     main ="DEN distribution")

#SFO
assignment_SFO <- assignment_only %>% 
  filter(HUB_ass == "SFO_dist") %>% distinct()

connections_matrix_SFO <- as.matrix(assignment_SFO)
g <- graph_from_data_frame(connections_matrix_SFO, directed=FALSE)
plot(g,
     vertex.size=2,
     edge.arrow.size = 0.05,
     layout = layout_nicely,
     main ="SFO distribution")

#ALL
assignment_all <- assignment_only %>% distinct()
HUBs_connections_a <- matrix (c("ORD_dist" , "ORD_dist" , "ORD_dist", "ORD_dist", "EWR_dist", "EWR_dist", 
                                "EWR_dist", "DEN_dist", "DEN_dist","SFO_dist"), ncol=1)
colnames(HUBs_connections_a) = "airport2"
HUBs_connections_b <- matrix (c("EWR_dist", "DEN_dist",  "SFO_dist", "IAH_dist", "DEN_dist", "SFO_dist",
                                "IAH_dist", "SFO_dist", "IAH_dist", "IAH_dist"), ncol =1)
colnames(HUBs_connections_b) = "HUB_ass"
HUBs_connections <- cbind(HUBs_connections_a, HUBs_connections_b)
View(HUBs_connections)
assignment_all <- rbind(assignment_all, HUBs_connections)

connections_matrix_all <- as.matrix(assignment_all)
g <- graph_from_data_frame(connections_matrix_all, directed=FALSE)
plot(g,
     vertex.size=2,
     edge.arrow.size = 0.05,
     layout = layout_nicely,
     main ="United Airlines - Optimized Parts Network")


###############CH9: HUB CONNECTIVITY####################
average_monthly_HUBs <- data_fligths_UA %>%
  group_by(month, O_D_pairs) %>%
  count(month, O_D_pairs) %>%
  group_by(O_D_pairs) %>%
  summarise(avg = mean(n))%>%
  filter(O_D_pairs %in% c("ORD - EWR", "ORD - DEN", "ORD - SFO", "ORD - IAH", "EWR - ORD", "EWR - IAH", "EWR - SFO","EWR - DEN",
                          "DEN - ORD", "DEN - EWR", "DEN - IAH", "DEN - SFO", "SFO - EWR", "SFO - ORD", "SFO - DEN", "SFO - IAH",
                          "IAH - SFO", "IAH - ORD", "IAH - EWR", "IAH _ DEN"))

head(average_monthly_HUBs)

average_monthly_HUBs %>%
  ggplot(aes(x=O_D_pairs, y=avg) ) +
  geom_segment(aes(x=O_D_pairs ,xend=O_D_pairs, y=0, yend=avg), color="#69b3a2", lwd=2) +
  geom_point(size=5, color="dark green") +
  coord_flip() +
  theme_gdocs() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("Origin-Destination Pairs") +
  ylab("Average Monthly Flights") +
  labs(title = "HUBs Connectivity - United Airlines", subtitle= "Connectivity between selected distribution HUBs") 
  scale_y_continuous(limits=c(0, 450))

