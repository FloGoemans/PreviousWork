#--------------------------------------------------------#
# Response_alluvial.R
#
# Response plot
# Author: Flo Goemans
#--------------------------------------------------------#

#--------------------------------------------------------#
# Read in data/ use from environment
#--------------------------------------------------------#

#adsl should already be in environment if running from Runall

#--------------------------------------------------------#
# Select data
#--------------------------------------------------------#

tier2 <- adsl[adsl$trt01a == "Tier 2", ]
#factorise response
tier2$ovrresp3 <- factor(tier2$ovrresp3)
tier2$ovrresp12 <- factor(tier2$ovrresp12)

#--------------------------------------------------------#
# Tabulate counts + set up for plot
#--------------------------------------------------------#

resptab <- as.data.frame(table(tier2$ovrresp3, tier2$ovrresp12, useNA="always"))

#separate out and set vertically
resp1 = resptab %>%
  select(Var1, Freq) %>%
  rename("Response" = "Var1") %>%
  mutate(id = row_number()) %>%
  mutate(visit = "3 Months")

resp2 = resptab %>%
  select(Var2, Freq) %>%
  rename("Response" = "Var2") %>%
  mutate(id = row_number()) %>%
  mutate(visit = "12 Months")

resp_all = resp1 %>%
  bind_rows(resp2)

#--------------------------------------------------------#
# Format variables
#--------------------------------------------------------#

resp_all$Response <-factor(resp_all$Response, labels =c("Yes", "No", "Not sure", "Missing"), exclude=NULL)
resp_all$Response <-factor(resp_all$Response, levels =c("No", "Not sure", "Yes", "Missing"))
resp_all$visit <-factor(resp_all$visit, levels= c("3 Months", "12 Months"), labels= c("3 Months", "12 Months"))


#--------------------------------------------------------#
# Draw graph
#--------------------------------------------------------#

jpeg(file=paste0(analpath, "output/res_alluvial.jpg))

ggplot(resp-all, 
	aes(x = visit, 
 	    stratum = Response,
	    alluvium = id, 
	    y = Freq,
	    fill = Response,
	    label = Response )) +
	scale_x_discrete(expand = c(.1,.1)) +
	geom_flow() +
	geom_stratum(alpha=.5) + 
	geom_text(stat = "stratum", size = 3) +
	scale_y_continuous("Number of Subjects", breaks=c(500, 1000, 1500, 2000, 2500)) +
	theme(legend.position = "none") + 
	ggtitle("Overall recovery response changes")

dev.off()

## End of program
