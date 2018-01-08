# working script to analysis audit data

library(ggplot2)
library(plotly)
packageVersion('plotly')

rvi1516 <- read.csv("data/Audit1516.csv")
rvi1617 <- read.csv("data/Audit1617.csv")
rvi1718 <- read.csv("data/Audit1718.csv")

rvi1516 <- as.data.frame(rvi1516)
rvi1617 <- as.data.frame(rvi1617)
rvi1718 <- as.data.frame(rvi1718)

# combine years for numbers tested
test <- merge(rvi1516, rvi1617, by = "Directorate", all.x = TRUE, all.y = TRUE)
combined_years <- merge( test, rvi1718, by = "Directorate", all.x = TRUE, all.y = TRUE)

num_tested <- cbind.data.frame(combined_years$Directorate, combined_years$No..tested.x,combined_years$No..tested.y, combined_years$No..tested)
colnames(num_tested) <- c("Directorate", "num_tested1516", "num_tested1617", "num_tested1718")
#rownames(num_tested) <- num_tested[,1]
#num_tested[,1] <- NULL
#num_tested[1,] <- NULL


# plot numbers tested each year

g <- plot_ly(num_tested, x = ~Directorate, y  = ~num_tested1516, type = 'bar', name = '15/16') %>%
  add_trace(y = ~num_tested1617, name = '16/17') %>%
  add_trace(y = ~num_tested1718, name = '17/18') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group', title = "Number tested")
g


#  % carriage and % positivity
percent_carry <- cbind.data.frame(combined_years$Directorate, combined_years$X.carrier.x,combined_years$X.carrier.y, combined_years$X.carrier)
colnames(percent_carry) <- c("Directorate", "percent_carry1516", "percent_carry1617", "percent_carry1718")

g <- plot_ly(percent_carry, x = ~Directorate, y  = ~percent_carry1516, type = 'bar', name = '15/16') %>%
  add_trace(y = ~percent_carry1617, name = '16/17') %>%
  add_trace(y = ~percent_carry1718, name = '17/18') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group', title = "Percentage carriers")
g

percent_pos <- cbind.data.frame(combined_years$Directorate, combined_years$X..positive.x,combined_years$X..positive.y, combined_years$X..positive)
colnames(percent_pos) <- c("Directorate", "percent_pos1516", "percent_pos1617", "percent_pos1718")

g <- plot_ly(percent_pos, x = ~Directorate, y  = ~percent_pos1516, type = 'bar', name = '15/16') %>%
  add_trace(y = ~percent_pos1617, name = '16/17') %>%
  add_trace(y = ~percent_pos1718, name = '17/18') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group', title = "Percentage positive")
g


# % of patients who do not get a result
# first calculate percentage with insufficient sample 
percent_nottested <- cbind.data.frame(combined_years$Directorate, (combined_years$No..sent.x - combined_years$No..tested.x)/combined_years$No..sent.x,
                                (combined_years$No..sent.y - combined_years$No..tested.y)/combined_years$No..sent.y,
                                (combined_years$No..sent - combined_years$No..tested)/combined_years$No..sent)
colnames(percent_nottested) <- c("Directorate", "percent_nottested1516", "percent_nottested1617", "percent_nottested1718")

g <- plot_ly(percent_nottested, x = ~Directorate, y  = ~percent_nottested1516, type = 'bar', name = '15/16') %>%
  add_trace(y = ~percent_nottested1617, name = '16/17') %>%
  add_trace(y = ~percent_nottested1718, name = '17/18') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group', title = "Percentage not tested (reason not given) ")
g

percent_insuff <- cbind.data.frame(combined_years$Directorate, combined_years$INSUFF.x/combined_years$No..sent.x,
                                   combined_years$INSUFF.y/combined_years$No..sent.y,
                                   combined_years$INSUFF/combined_years$No..sent)
colnames(percent_insuff) <- c("Directorate", "percent_insuff1516", "percent_insuff1617", "percent_insuff1718")

g <- plot_ly(percent_insuff, x = ~Directorate, y  = ~percent_insuff1516, type = 'bar', name = '15/16') %>%
  add_trace(y = ~percent_insuff1617, name = '16/17') %>%
  add_trace(y = ~percent_insuff1718, name = '17/18') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group', title = "Percentage not tested (insufficient sample)")
g




