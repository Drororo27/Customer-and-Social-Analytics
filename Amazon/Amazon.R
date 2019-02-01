# Amazon 
# Social Network Team assignment

rm(list=ls())
setwd("~/Documents/UCI/Customer and Social Analytics/Amazon Assignment")
product = read.csv("products.csv")
copurchase = read.csv("copurchase.csv")

################################################## Q1 ##################################################
# delelet not book product, the books with salesrank>150,000, and salesrank = -1 in product file
library(dplyr)
product_book = filter(product, group == "Book" & salesrank != -1 & salesrank <= 150000)
summary(product_book$group)

# delelet not book product in copurchase
id_not_need = which(!(copurchase$Target %in% product_book$id) | !(copurchase$Source %in% product_book$id))
copurchase_book = copurchase[-id_not_need,]
rm(id_not_need)
#check if the result is correct
setdiff(union(copurchase_book$Source,copurchase_book$Target), product_book$id)

#alternative
#copurchase_book = filter(copurchase, copurchase$Target %in% product_book$id, 
#                         copurchase$Source %in% product_book$id)


################################################## Q2+Q3 ##################################################
library("igraph")
#Build network
net = graph.data.frame(copurchase_book, directed = T)

#Compute the degree
node_degree = data.frame(name_node = V(net)$name,
                         all_degree = degree(net, mode = "all"),
                         in_degree = degree(net, mode = "in"),
                         out_degree = degree(net, mode = "out"))

################################################## Q4 ##################################################
#Find the product with highest degree
node_degree[which(node_degree$all_degree == max(node_degree$all_degree)),]

#find its subcomponent
sub1 = subcomponent(net, "33", "all")
sub2 = subcomponent(net, "4429", "all")

#check whether two subcomponents are the same
setdiff(as.integer(as_ids(sub1)),as.integer(as_ids(sub2))) 

#Build a new network based on the subcomponent
net2 = induced.subgraph(net, sub1, impl = "auto")

################################################## Q5 ##################################################
#compute diameter
D = diameter(net2, directed = T, weights = NA)
#find the component vertex of diameter
D1 = get_diameter(net2, directed = T, weights = NA)

#color all the vertex
V(net2)$color = "gray"
#colorthe vertex on diameter
V(net2)[D1]$color = "yellow"
#colorthe vertex has highest degree
V(net2)["33"]$color = "red"
V(net2)["4429"]$color = "red"

#give size to all the vertex
V(net2)$size = 1
#give a larger size to the vertex on diameter
V(net2)[D1]$size = 3
#give largerest size to the vertex has highest degree
V(net2)["33"]$size = 4
V(net2)["4429"]$size = 4

# Add some specific lables and legends.
V(net2)["33"]$bookname = "Double Jeopardy"
V(net2)["4429"]$bookname = "Harley-Davidson Panheads"

#set.seed(111)
set.seed(333)
plot(net2, 
     vertex.color = V(net2)$color, 
     vertex.size = V(net2)$size, 
     edge.arrow.size = 0.05, 
     vertex.label = V(net2)$bookname,
     vertex.label.dist = 10,
     vertex.label.color = "black",
     vertex.label.degree = pi,
     vertex.label.cex = 1, 
     layout=layout.lgl,     
     main = "Subcomponent")

legend(x=-0.4, y=-0.5, c("Nodes with highest degree","Nodes along the diameter", "Normal nodes"), pch=21,
       col="#777777", pt.bg=c("red","yellow","gray"), pt.cex=2, pt.lwd = 1, bty="n", ncol=1)

#Try different layout options:
#layout.circle
#layout.kamada.kawai
#layout.fruchterman.reingold
#layout.lgl

################################################## Q6 ##################################################
#degree_distribution
deg = degree(net2, mode="all")
deg.dist = degree_distribution(net2, cumulative = T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")
hist(deg, breaks=20, main="Histogram of node degree")

#density
edge_density(net2, loops = FALSE)

#degree centrality
centr_degree_tmax(net2, mode = "all", loops = FALSE)

#in/out/all degree, closeness centrality, between centrality, and hub/authority scores 
#for each node
network_attributes = data.frame(
  name_node2 = V(net2)$name,
  all_degree=degree(net2, mode = "all"), 
  in_degree=degree(net2, mode = "in"), 
  out_degree=degree(net2, mode = "out"),
  Closeness = closeness(net2, mode = "all", weights = NA, normalized = FALSE),
  Betweenness = betweenness(net2, directed = T, weights = NA),
  Hub_score = hub_score(net2)$vector,
  Authority_score = authority.score(net2)$vector
  )

################################################## Q7 ##################################################

#Merge and create a new data frame
product_sub = merge(product_book, network_attributes, by.x = "id", by.y = "name_node2")

#nghb_mn_rating, nghb_mn_salesrank, and nghb_mn_review_cnt
for(i in 1:length(product_sub$id)){
  id_neighbors = as.integer(as_ids(neighbors(net2,as.character(product_sub$id[i]),"in")))
  
  product_sub$nghb_mn_rating[i] = product_sub$rating[which(product_sub$id %in% id_neighbors)] %>%
    mean()
  
  product_sub$nghb_mn_salesrank[i] = product_sub$salesrank[which(product_sub$id %in% id_neighbors)] %>%
    mean()
  
  product_sub$nghb_mn_review_cnt[i] = product_sub$review_cnt[which(product_sub$id %in% id_neighbors)] %>%
    mean()
}

#check vertex "77"
as.integer(as_ids(neighbors(net2,"77","in")))
which(product_sub$id %in% as.integer(as_ids(neighbors(net2,"77","in"))))

################################################## Q8 ##################################################

fit1 <- glm(salesrank ~ review_cnt + downloads + rating + all_degree + in_degree + out_degree + Closeness 
            + Betweenness + Hub_score + Authority_score 
            + nghb_mn_rating + nghb_mn_salesrank + nghb_mn_review_cnt, 
            data = product_sub, family = poisson(), na.action = na.exclude)
summary(fit1)

#delete all_degree since the liner relationship between all_degree, in_degree, and out_degree
fit2 <- glm(salesrank ~ review_cnt + downloads + rating + in_degree + out_degree + Closeness 
            + Betweenness + Hub_score + Authority_score 
            + nghb_mn_rating + nghb_mn_salesrank + nghb_mn_review_cnt, 
            data = product_sub, family = poisson(), na.action = na.exclude)
summary(fit2)

#Performing the deviance goodness of fit test
pchisq(fit2$deviance, df=fit2$df.residual, lower.tail=FALSE)

#Diagnostics
plot(fit2)

#interpretation of coefficient
exp(coef(fit2))


# robust
library(sandwich)
cov.output = vcovHC(fit2, type = "HC0")
std.err = sqrt(diag(cov.output))
robust = cbind(Estimation = coef(fit2), "Robust Std Err" = std.err, 
               "Pr(>|z|)" = 2*pnorm(abs(coef(fit2)/std.err), lower.tail = FALSE),
               LL = coef(fit2) - 1.96 * std.err,
               UL = coef(fit2) + 1.96 * std.err)


