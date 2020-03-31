# Social Network Analysis Assignment
# Date:   17-Feb-2020
# Author: Sonal Mendiratta
#install.packages("installr") 
#library(installr)
#updateR()

#Load relevant packages
#install.packages("psych")
#install.packages("sqldf")
#install.packages('rlang')

install.packages("tidyr")

library(tidyr)
library(igraph)
library(dplyr)  
library("psych")
library(sqldf)



#setting working directory
setwd("E:/a.UCI/1.Winter Quarter/Customer and Social Analytics/Class6/HW")

###################################################################################
#### DATA EXPLORATION ####
###################################################################################
#Importing both the files
nodes <- data.frame(read.csv("products.csv"))
links <- data.frame(read.csv("copurchase.csv"))

#Looking at the files
head(nodes)
#id                                                             title group salesrank review_cnt downloads rating
#1  1                           Patterns of Preaching: A Sermon Sampler  Book    396585          2         2    5.0
#2  2                                        Candlemas: Feast of Flames  Book    168596         12        12    4.5
#3  3                  World War II Allied Fighter Planes Trading Cards  Book   1270652          1         1    5.0
#4  4      Life Application Bible Commentary: 1 and 2 Timothy and Titus  Book    631289          1         1    4.0
#5  5                   Prayers That Avail Much for Business: Executive  Book    455160          0         0    0.0
#6  6 How the Other Half Lives: Studies Among the Tenements of New York  Book    188784         17        17    4.0

head(links)
#Source Target
#1      1      2
#2      1      4
#3      1      5
#4      1     15
#5      2     11
#6      2     12

nrow(nodes) 
#[1] 259167
nrow(links)
#[1] 1234870

#distribution of the group variable - we only need to focus on Books
table(nodes$group)
#Baby Product         Book           CE          DVD        Music     Software          Toy        Video  Video Games 
#1                    188013            3         9554        48993            2            3        12597            1 

#getting unique number of combinations of source and target from the copurchase data
nrow(unique(links[,c("Source", "Target")]))
#[1] 1234870

################################################################################
#                     ASSIGNMENT SOLUTIONS 
################################################################################


################################################################################
#                          QUESTION 1 
#   Delete products that are not books from “products” and “copurchase” files. 
#   And then delete the books with salesrank>150,000 or salesrank = -1.
################################################################################

#filtering out books from purchase table (defined as nodes above)
nodes_book <- filter(nodes, group == "Book")
nrow(nodes_book)
#[1] 188013

#keeping only those books which have salesrank > 150000 or salesrank = -1
nodes_book_sub <- filter(nodes_book, salesrank <= 150000 & salesrank != -1 ) 
nrow(nodes_book_sub)
#[1] 35250


#keeping data only for these products in the copurchase table now (defined as links above)
links_book_sub <- sqldf("select * 
                        from links
                        where Source in (select distinct id from nodes_book_sub)", row.names=TRUE)

links_book_sub1 <- sqldf("select * 
                        from links_book_sub
                        where Target in (select distinct id from nodes_book_sub)", row.names=TRUE)
head(links_book_sub1)
nrow(links_book_sub1)
#[1] 22460

#tables to be used now : nodes_book_sub and links_book_sub1

################################################################################
#                          QUESTION 2
#   Create a variable named in-degree, to show how many “Source” products 
#   people who buy “Target” products buy; i.e. how many edges are to the focal 
#   product in “co-purchase” network.
################################################################################

#First converting the data into an igraph object
#net <- graph_from_data_frame(d=links_book_sub1, vertices=nodes_book_sub, directed=T) 

net <- graph.data.frame(d=links_book_sub1, directed=T) 

#in_degree
net$in_degree <- degree(net, mode="in")

#plotting it on a graph
hist(in_degree, col='pink',ylab='Frequency', main="Histogram of in-degree")

################################################################################
#                          QUESTION 3
#   Create a variable named out-degree, to show how many “Target” products 
#   people who buy “Source” product also buy; i.e., how many edges are 
#   from the focal product in “co-purchase” network.
################################################################################

#out_degree

net$out_degree <- degree(net, mode="out")

#plotting it on a graph
hist(out_degree, col='purple',ylab='Frequency', main="Histogram of out-degree")
out_degree

################################################################################
#                          QUESTION 4
#   Pick up one of the products (in case there are multiple) with highest 
#   degree (in-degree + out-degree), and find its subcomponent, i.e., 
#   all the products that are connected to this focal product. 
#   From this point on, you will work only on this subcomponent. .
################################################################################

#all_degrees
net$all_degree <- degree(net, mode="all")
#plotting it on a graph
hist(all_degree, col='green',ylab='Frequency', main="Histogram of node degree")

#finding product with the highest degree
V(net)$name[all_degree==max(all_degree)]
#[1] "33"   "4429"

head(sort(all_degree,decreasing = TRUE))
#product ID 33 and 4429 have the highest degree of 53
#33 4429  244  302 5913 2501 
#53   53   36   22   22   21 

#Picking up the product id - 33 for further questions and analysis

sub  = subcomponent(net, "33", mode = "all") #displays the list of nodes

link_33_a <- filter(links_book_sub1, Source == "33") 
nrow(link_33_a) #0 

link_33 <- filter(links_book_sub1, Target == "33") 
nrow(link_33) #53
#This means that all nodes are inwards to the node with product id = 33 
#using link_33 table now

#nodes_33 <-  sqldf("select * 
#                        from nodes_book_sub
#                        where id in (select distinct Source from link_33
#                                      union
#                                     select distinct Target from link_33)", row.names=TRUE)

#########################################################
##Question 5	Visualize the subcomponent using iGraph
########################################################
#This step is to create a sub object of graph based on parent dataset and targeted sub dataset
subnet <- induced_subgraph(net, sub)


#labeling out the names and degrees
V(subnet)$label <- V(subnet)$name 
V(subnet)$degree <- degree(subnet)


plot(subnet,
     vertex.color='yellow',
     edge.color='orange',
     vertex.size= V(subnet)$degree*0.3,
     edge.arrow.size=0.03,
     vertex.label.cex=0.01,
     layout=layout.kamada.kawai)


#######################################
#Question6 Statistics of subnet
#######################################

#1 degrees
in_degree = degree(subnet, mode="in")
hist(in_degree, col='pink',ylab='Frequency', main="Histogram of in-degree")
#in degree distribution is highly skewed, we might need to provide log transformation later on in the data

out_degree = degree(subnet, mode="out")
hist(out_degree, col='purple',ylab='Frequency', main="Histogram of out-degree")
#out degree seems ok

all_degree = degree(subnet, mode="all")
hist(all_degree, col='purple',ylab='Frequency', main="Histogram of out-degree")
#all degree is highly skewed as well, doing log transformation in a bit


#2 density
edge_density(subnet, loops=FALSE)
#0.001436951, this means it's not a very dense net, in other words, there could possibly be a lot more edges within the net


#3 diameter
diameter(subnet, directed=F, weights=NA)
#diameter equals to 41 so the maximum longest edges in between two nodes is 41.

reciprocity(graph)

#4 Closeness (centrality based on distance to others in the graph)
sort(closeness(subnet, mode='all', weights=NA), decreasing = TRUE)
#undoubtedly, point 33, which is our centroid, is the closest to all 

#5
# Betweenness (centrality based on a broker position connecting others)
#Number of geodesics that pass through the node or the edge
sort(betweenness(subnet, directed='T', weights=NA), decreasing = TRUE)
#very interestingly, we can see that node "2501", which is "The Narcissistic Family", this means this book serves the best
#as an "intermediate" of the network, a lot of nodes pass it, in the business meaning, a lot of books get purchased because
#at some point some users purchased "The Narcissistic Family"


#6 HUB/AUTHORITY SCORE
hs <- hub.score(subnet)$vector
as <- authority.score(subnet)$vector

sort(hs, decreasing = TRUE)
#the hub is 195144, which is "Paradise Found : Growing Tropicals in Your Own Backyard", this book is authority means
#that a lot of traffic are coming out from this book, making this book a initiation "source"

sort(as, decreasing = TRUE)
#the authority is our centroid book again, which means all the books leads to this book eventually, making this the "sink"
#of the network, which means all buyers ended up buying this book


##############
#Q7
##############
#1
sub_df <- as.data.frame(get.edgelist(subnet))
#making subnet back to a df, and export all the relationships.
#directly using original datatable of "copurchase" won't work because we need all the inbetween nodes as well
#V1 is Source and V2 is Target
sub_df

#2
sub_df_joined <- sqldf("select sub_df.V2 as 'Target', sub_df.V1 as 'Source', nodes.rating as src_rating, nodes.salesrank as src_rank, nodes.review_cnt as src_rvcnt
                        from sub_df
                        join nodes
                        on sub_df.V1 = nodes.id")
#This step we join the data with their ratings/ranks etc back with the SOURCE, and then groupby TARGET.
#So that we will have all the source scores within each Target group...hope the logic is right


#3 Joining back after groupby
rating <-sub_df_joined %>%
                        group_by(Target) %>%
                        summarize(nghb_mn_rating = mean(src_rating, na.rm=TRUE))

sub_df_joined1 <- sqldf("select sub_df_joined.*, rating.nghb_mn_rating
                        FROM sub_df_joined
                        LEFT JOIN rating
                        on sub_df_joined.Target = rating.Target")


srank <-sub_df_joined1 %>%
        group_by(Target) %>%
        summarize(nghb_mn_salesrank = mean(src_rank, na.rm=TRUE))

sub_df_joined2 <- sqldf("select sub_df_joined1.*, srank.nghb_mn_salesrank
                        FROM sub_df_joined1
                        LEFT JOIN srank
                        on sub_df_joined1.Target = srank.Target")

vcnt <-sub_df_joined2 %>%
        group_by(Target) %>%
        summarize(nghb_mn_review_cnt = mean(src_rvcnt, na.rm=TRUE))

sub_df_joined_3 <- sqldf("select sub_df_joined2.*, vcnt.nghb_mn_review_cnt
                        FROM sub_df_joined2
                        LEFT JOIN vcnt
                        on sub_df_joined2.Target = vcnt.Target")

in_degree = degree(net, mode="in")
out_degree = degree(net, mode="out")

#sub_df_joined_3 is our data to run model for Q8

in_degree
as_data_frame(net$in_degree, what = "both")
