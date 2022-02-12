#the cleaning process
#libraries
setwd("C:/Users/John/OneDrive/whatsapp study")
library(rwhatsapp)
library(dplyr)
library(tidyr)
chat=rwa_read("WhatsApp Chat with kplc (4).txt")%>%
  filter(!is.na(author))
View(chat)
#getting only the text
token=chat%>%
  select(text)
View(token)
#removing other parts of the chat without kplc
#and remaining with ones that have actual tokens
token_clean=token%>%
  filter(grepl('KPLC',text))
#separating the texts using some rule, in this case :
#first specify the column names
token3=token_clean%>%
  separate(text,c("info","mtr","token","date","hour","units","amt","amt2","token amt"),
           ":",extra="merge")
View(token3)
#remove the text accompanying the separation you have just made
token3$token=gsub("Date","",token3$token)
token3$mtr=gsub("Token","",token3$mtr)
token3$hour=gsub("Units","",token3$hour)
token3$units=gsub("Amt Ksh","", token3$units)
token3$amt=gsub("Token Amt","", token3$amt)
token3$amt2=gsub("VAT","", token3$amt2)
token3$info=gsub("MtrNo","",token3$info)
View(token3)

#some tokens do not have the hyphen, remove that
token4=token3%>%
  filter(grepl('-',token))
#some tokens have spaces before the start of token
#remove that
token4$token=gsub(" ","",token4$token)
#token4$token=gsub(" UnitskWh","",token4$token)

token4=token4%>%
  filter(!grepl('Units',token))
#your interest is the 20 digit code for tokens
#so get only that from your data
#we will use the other variables as well but later
token_digits=strsplit(token4$token,"")  
View(token_digits)
#you want a data frame
digits2=data.frame(t(sapply(token_digits,`[`)))
View(digits2)
#time to remove the hyphens from the data frame
digits3=digits2%>%
  select(-c(X5,X10,X15,X20,X25))

#now rename the columns
colnames(digits3)=c("one","two","three","four","five","six","seven",
                    "eight","nine","ten","eleven","twelve","thirteen",
                    "fourteen","fifteen","sixteen","seventeen",
                    "eighteen","nineteen",
                    "twenty")
#now that you have your data set, check if it is numeric
View(digits3)

str(digits3)
#now convert to numeric
digits3_num=data.frame(lapply(digits3,as.numeric))
write.csv(digits3_num,"token data digits.csv")
View(digits3_num)
str(digits3_num)
View(digits3_num)
#now sum all the digits or basically all the columns
digits3_num$sum=cbind(rowSums(digits3_num))

hist(digits3_num$sum)


#check for correlations
cormat <- round(cor(digits3_num),2)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
#correlation heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


#association mining
install.packages("arules")
install.packages("arulesViz")

# Loading package
library(arules)
library(arulesViz)

# Fitting model
# Training Apriori on the dataset
set.seed = 220 # Setting seed
associa_rules = apriori(data = digits3_num, 
                        parameter = list(support = 0.004, 
                                         confidence = 0.2))

# Plot
itemFrequencyPlot(dataset, topN = 10)

# Visualising the results
inspect(sort(associa_rules, by = 'lift')[1:10])
plot(associa_rules, method = "graph", 
     measure = "confidence", shading = "lift")