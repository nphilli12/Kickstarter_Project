title: "Exploratory Analysis of Kickstarter Data"
author: "Ashley Kittrell"
date: "Last Updated: November 25, 2018"

install.packages (c('caret','skimr','RANN','randomforest','fastadaboost','gbm','xgboost','caretEnsemble','c50','earth'))
install.packages("devtools", "rworldxtras", "ggplot2")

install.packages("tidyverse")
install.packages("ggthemes")
install.packages("lubridate")
install.packages("rworldmap")
install.packages("knitr")
library(tidyverse)
library(ggthemes)
library(lubridate)
library(rworldmap)
library(ggplot2)
library(knitr)

kickstarter <- read.csv("KickStarterData.csv", header = T)

str(kickstarter)
kable(head(kickstarter, 5))
kable(tail(kickstarter, 5))

##clean data (remove na)
sapply(kickstarter, function(x) sum(is.na(x)))

kickstarter <- kickstarter[,-16]
colnames(kickstarter)[16] <- "usd_pledged"
colnames(kickstarter)[17] <- "usd_goal"


#What types of projects are most popular?
cat.freq <- kickstarter %>%
  group_by(main_category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

cat.freq$main_category <- factor(cat.freq$main_category, levels=cat.freq$main_category)

ggplot(cat.freq, aes(main_category, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Category") + xlab("Project Category") + ylab("Frequency") + 
  geom_text(aes(label=count), vjust=-0.5) + theme_classic() +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

##Now let's look at the Condensed categories
cat.freq <- kickstarter %>%
  group_by(Main_category_condensed) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

cat.freq$Main_category_condensed <- factor(cat.freq$Main_category_condensed, levels=cat.freq$Main_category_condensed)

ggplot(cat.freq, aes(Main_category_condensed, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Condensed Category") + xlab("Condensed Category
1=Creative Design, 2=Media, & 3=Tech") + ylab("Frequency") + 
  geom_text(aes(label=count), vjust=-0.5) + theme_classic() +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="green1", high="green4")
##Film & Video* appears to be the most popular project category and *Dance* the least popular.
#What types of projects are being funded?

##Now let's taking a look at the top 15 highest funded projects.
kable(head(kickstarter, 5))
kable(head(kickstarter[order(-kickstarter$usd_pledged), c(2,5,16)], 17))

##A lot of the projects here fall under the Design subcategory.

##Now let's list the top 15 most backed projects. . .


kable(head(kickstarter[order(-kickstarter$backers), c(2,3,13)], 17))

##The most common subcategory here appears to be Games.

##Now let's determine what types of projects funding is going towards. We'll do this by aggregating the amount of funds pledged for each category, providing us with the total amount pledged for each category.

pledged.tot <- kickstarter %>%
  group_by(main_category) %>%
  summarize(total=sum(usd_pledged)) %>%
  arrange(desc(total))

pledged.tot$main_category <- factor(pledged.tot$main_category, levels=pledged.tot$main_category)

ggplot(pledged.tot, aes(main_category, total/1000000, fill=total)) + geom_bar(stat="identity") + 
  ggtitle("Total Amount Pledged by Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD millions)") + 
  geom_text(aes(label=paste0("$", round(total/1000000,1))), vjust=-0.5) + theme_classic() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="orange1", high="orange3")

##*Games*, *Design*, and *Technology* are the highest grossing categories by far.

##It's important to take the number of backers into account as well, so let's determine the average amount pledged per backer for each category. We'll calculate this by taking the total amount pledged for each category and dividing it by the total number of backers for each category.


pledged.avg <- kickstarter %>%
group_by(main_category) %>%
summarize(pledged=sum(usd_pledged), backers=sum(backers)) %>%
mutate(avg=pledged/backers) %>%
arrange(desc(avg))

pledged.avg$main_category <- factor(pledged.avg$main_category, levels=pledged.avg$main_category)

ggplot(pledged.avg, aes(main_category, avg, fill=avg)) + geom_bar(stat="identity") + 
ggtitle("Average Amount Pledged per Backer") + xlab("Project Category") + 
ylab("Amount Pledged (USD)") + 
geom_text(aes(label=paste0("$", round(avg,2))), vjust=-0.5) + theme_classic() + 
theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
scale_fill_gradient(low="red1", high="red4")

##Technology* has the highest average amount pledged, whereas *Comics* has the lowest average amount pledged. An interesting note here is that the average amount pledged for *Technology* is nearly double that of *Games*, even though *Games* had the higher aggregate amount pledged as shown in the previous graph.

##Next we'll examine the distribution of amounts pledged for individual projects using box plots. There are no doubt a lot of projects that received little to no funding as well as huge outliers, which will cause the box plots to appear "flat" near the bottom.

ggplot(kickstarter, aes(main_category, usd_pledged, fill=main_category)) + geom_boxplot() + 
  ggtitle("Amount Pledged vs. Project Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD)") + 
  theme(plot.title=element_text(size=15, face="bold", hjust=0.5), 
        axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  coord_cartesian(ylim=c(0,20000))

##*Design* and *Games* have a high upper quartile compared to other categories. *Design*, *Dance*, and *Theater* have a high median amount pledged relative to other categories. *Comics* has a surprisingly high upper quartile and median, even though it had a lower amount pledged (both in aggregate and average terms) relative to other categories, suggesting that the average number of backers must be relatively high for each individual project in this category. *Technology* on the other hand has a low median amount pledged, despite having a high amount pledged (both in aggregate and average terms), suggesting that there are a lot of high outliers. *Crafts* and *Journalism* have a very small IQR and low median compared to the other categories.

#How much funding is required?

##Having looked at the amounts pledged in funds, we will turn to the funding goals for projects. Let's begin by examining the top 15 most ambitious projects, that is, projects with the highest set funding goals.
kable(head(kickstarter))
kable(head(kickstarter[order(-kickstarter$usd_goal), c(2,3,17,11)], 17))

##Aside from the one project that was suspended, all other projects failed here. Their goals must have been set too high and seen as being too unreasonable for the idea they were selling. Let's look at the top 15 most ambitious projects that were successfully funded instead.

goal.tops <- kickstarter[kickstarter$Status=="successful",]

kable(head(goal.tops[order(-goal.tops$usd_goal), c(2,3,17,11)], 17))

##Some projects listed here were also found in the top 15 highest funded projects list as well. The most common subcategories listed here are *Video Games* and *Narrative Film*.

##Let's look at the average project goal for each category. Perhaps it will give us some insight into what types of projects were successful or unsuccessful later on.


goal.avg <- kickstarter %>%
group_by(main_category) %>%
summarize(goal=sum(usd_goal), projects=n()) %>%
mutate(avg=goal/projects) %>%
arrange(desc(avg))

goal.avg$main_category <- factor(goal.avg$main_category, levels=goal.avg$main_category)

ggplot(goal.avg, aes(main_category, avg, fill=avg)) + geom_bar(stat="identity") + 
ggtitle("Average Project Goal") + xlab("Project Category") + ylab("Project Goal (USD)") + 
geom_text(aes(label=paste0("$", round(avg,0))), vjust=-0.5) + theme_classic() + 
theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
scale_fill_gradient(low="pink1", high="pink3")

##*Technology*, *Journalism*, and *Film & Video* have the highest average project goals by far. On the opposite side, we find *Dance*, *Crafts*, and *Photography*.

##We'll examine the distribution of project goal amounts for individual projects using box plots here as well.

ggplot(kickstarter, aes(main_category, usd_goal, fill=main_category)) + geom_boxplot() + 
  ggtitle("Project Goal vs. Project Category") + xlab("Project Category") + 
  ylab("Project Goal (USD)") + 
  theme(plot.title=element_text(size=15, face="bold", hjust=0.5), 
        axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  coord_cartesian(ylim=c(0,60000))

##*Technology* has an incredibly high upper quartile and median. Although not nearly as high, *Design* and *Food* also have relatively high upper quartile and median values as well. The average project goal for these two categories was lower than that of *Journalism* and *Film & Video*, but they have higher median and upper quartile values, suggesting that the former must have many projects with smaller goal amounts, the latter must have many high outliers, or both.

##Having looked at both the amount pledged and goal for different kinds of projects, let's see how the distribution of each compares to one another. Since we can expect both distributions to be heavily right-skewed due to many projects that received little to no funding and extremely high outliers, we will use a log transformation on both variables to better visualize their distributions.

usd.amounts <- gather(kickstarter, type, amount, usd_pledged, usd_goal, factor_key=T)

ggplot(usd.amounts, aes(log(amount+1), fill=type)) + 
geom_histogram(alpha=0.5, position="identity") + 
ggtitle("Distribution of log(USD Pledged) vs. log(USD Goal)") + xlab("log(USD + 1)") + 
ylab("Frequency") + scale_fill_discrete("Type", labels=c("USD Pledged", "USD Goal"))

##USD Goal appears to be relatively normally distributed. USD Pledged on the other hand has a bimodal distribution, with a tall left peak, which would represent projects that received either little or no funding. The approximate centre of the distribution of USD Pledged is located to the left side of the distribution of USD Goal, illustrating how for the most part, many projects did not receive the required funding they were looking for.

##On the subject of comparing the amount pledged and the goal for each project, an interesting figure to look at would be the ratio of *usd_pledged* to *usd_goal*. This number would represent the multiplicative amount by which a project's funding exceeded its goal. Let's examine a list of the top 15 projects with the highest of such ratio.

kickstarter$ratio <- kickstarter$usd_pledged/kickstarter$usd_goal
kable(head(kickstarter))
kable(head(kickstarter[order(-kickstarter$ratio), c(2,3,16,17,20)], 20))

##Most of the projects here only have a goal of 1 USD. Let's only look at projects with a minimum goal of 1000 USD.

goal.min <- kickstarter[kickstarter$usd_goal>=1000,]

kable(head(goal.min[order(-goal.min$ratio), c(2,3,16,17,20)], 20))

##Again, we can recognize some of the projects here from the highest funded projects list from earlier. The most common projects listed here are *Tabletop Games* and *Product Design*.

#What types of projects were successful and unsuccessful?

##Let's break down the number of projects by their status (e.g. successful, failed, cancelled, etc.).


Status.freq <- kickstarter %>%
group_by(Status) %>%
summarize(count=n()) %>%
arrange(desc(count))

Status.freq$Status <- factor(Status.freq$Status, levels=Status.freq$Status)

ggplot(Status.freq, aes(Status, count, fill=count)) + geom_bar(stat="identity") + 
ggtitle("Projects by Status") + xlab("Project Status") + ylab("Frequency") + 
geom_text(aes(label=count), vjust=-0.5) + theme_classic() + 
theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
axis.text.x=element_text(size=12), legend.position="null") + 
scale_fill_gradient(low="skyblue1", high="royalblue4")

##More projects failed than succeeded. It seems most projects don't see the light of day. We can further group the projects into two different categories, "complete" projects (projects that have reached their deadline, i.e. successful and failed pojects) and "incomplete" projects (projects that have not reached their deadline, i.e. live, cancelled, or suspended projects). Let's do this and look at the project status proportion for each group.

Status.grp <- kickstarter %>%
filter(Status!="undefined") %>%
mutate(grp=ifelse(Status %in% c("successful", "failed"), "complete", "incomplete")) %>%
group_by(grp, Status) %>%
summarize(count=n()) %>%
mutate(pct=count/sum(count)) %>%
arrange(grp, desc(Status))

Status.grp$Status <- factor(Status.grp$Status, levels=Status.grp$Status)

ggplot(Status.grp, aes(grp, pct, fill=Status)) + geom_bar(stat="identity") + 
ggtitle("Project Status by Completion") + xlab("Project Completion") + ylab("Percentage") + 
scale_x_discrete(labels=c("Complete", "Incomplete")) + 
scale_y_continuous(labels=scales::percent) + 
scale_fill_brewer(name="Project Status", 
labels=c("Successful", "Failed"), 
palette="Set1") + 
geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5), 
colour="white", size=5) + theme_classic() + 
theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
axis.text.x=element_text(size=12), legend.position="bottom", 
legend.title=element_text(size=12, face="bold"))


##Now we know that approximately 60% of completed projects failed and only approximately 40% succeeded.

Status.pct <- kickstarter %>%
filter(Status %in% c("successful", "failed")) %>%
group_by(main_category, Status) %>%
summarize(count=n()) %>%
mutate(pct=count/sum(count)) %>%
arrange(desc(Status), pct)

Status.pct$main_category <- factor(Status.pct$main_category, 
levels=Status.pct$main_category[1:(nrow(Status.pct)/2)])

ggplot(Status.pct, aes(main_category, pct, fill=Status)) + geom_bar(stat="identity") + 
ggtitle("Success vs. Failure Rate by Project Category") + 
xlab("Project Category") + ylab("Percentage") + scale_y_continuous(labels=scales::percent) + 
scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
labels=c("Success", "Failure")) + 
geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5), 
colour="white", size=5) + theme_classic() + 
theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
axis.text.x=element_text(size=12), legend.position="bottom", 
legend.title=element_text(size=12, face="bold")) + coord_flip()


##Dance*, *Theater*, and *Comics* have the highest success rates and *Technology*, *Journalism*, and *Crafts* have the lowest. This agrees with the box plots for amounts pledged and project goal amounts above as *Dance* and *Comics* both had high median amounts pledged and low median goals, with *Theater* having a low median goal as well. *Technology*, *Journalism*, and *Crafts* had low median amounts pledged, with *Technology* having a high median goal. In general, the higher the amount pledged and/or the lower the goal, the more likely a project will be successful. Interestingly enough, *Crafts*, having a low median amount pledged, also has a low success rate despite having a low median goal, which may indicate that people generally are not as interested in this category as a whole.



##Projects by country -It would be nice to know what countries Kickstarter projects are originating from. We could create a table or a bar plot for the number of projects by country, but I think it would look much nicer if we used a geographical heat map instead.
install.packages(rworldmap)
countries.freq <- kickstarter %>%
filter(country!='N,0"') %>%
group_by(country) %>%
summarize(count=n())

countries.match <- joinCountryData2Map(countries.freq, joinCode="ISO2", nameJoinColumn="country")

mapCountryData(countries.match, nameColumnToPlot="count", 
mapTitle="Number of Projects by Country", catMethod="logFixedWidth", 
colourPalette="heat")
library(magrittr)
library(dplyr)
Kickstarter %>% group_by(main_category) %>% summarise(avg=mean(usd_goal_real))
##Projects seem to primarily originate from North America, Europe, and Oceania, with the odd Japan standing out from the rest of Asia. The United States, and to a lesser extent the United Kingdom, have the highest number of projects.
