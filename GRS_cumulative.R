################################################################
# Project: 		  MSc project.
# File name:	  Z:/My Documents/MSc Project/programs/PRS_cumulative.R
# Purpose:		 
#
# Author: 		  Flo Goemans
# Date: 		    4 Aug 2021
################################################################

#--------------------------------------------------------------#
# Setup Environment
#--------------------------------------------------------------#

# Set working directory
setwd("Z:/My Documents/MSc project")

# Install Packages needed


#load functions from installed packages
library(ROCit)
library(pROC)
library(ggplot2)
#--------------------------------------------------------------#
# Read in and process files
#--------------------------------------------------------------#

#---------------#
# Process file 1
#---------------#
file1 <- read.csv("./data/meta_beta_15_SNPs_v2.csv")
#save(file1, file="./data/beta_15_SNPs.RData")

#---------------#
# Process file 2
#---------------#
file2 <- read.csv("./data/UUS_data_15_SNP_v2.csv")
#save(file2, file="./data/UUS_15_SNP_data_v2.RData")

#---------------#
# Process file 3
#---------------#
file3 <- read.csv("./data/UUS_data_15_SNP_info.csv")

#order by chromosome and position
file3_s<- file3[ order(file3$chromosome, file3$position), ]
#save(file3_s, file="./data/UUS_15_SNP_info.RData")

#--------------------------------------------------------------#
# Create frames for PRS
#--------------------------------------------------------------#

#---------------#
# Merge file 1 & 3 - keeping those in both files - 15 SNPs
#---------------#
SNPs_all <- merge(file1, file3_s, by = c("chromosome", "position", "rsid"))

#drop extra SNP;
#SNPs_all <- SNPs_all[SNPs_all$X!=15, ] 

#---------------#
# Data checks and create beta for MAF
#---------------#

# add OR for beta - just for information
SNPs_all$OR <- exp(SNPs_all$beta)

## check allele freq in UUS data and add to all SNPs data
#setting up vector

colnam <- vector(mode="character", length=15)
rsid <- vector(mode="character", length=15)
freq <- vector(mode="double", length=15)

for(i in 1:15){
  #first column is now subject id - don't use
  colnam[i] <- colnames(file2[i+1])
  rsid[i]<- substr(colnam[i],1,nchar(colnam[i])-2)
  freq[i] <- sum(file2[,i+1])/ (nrow(file2)*2)
  
}
uus_freq <- cbind(rsid, freq)

SNPs_all <- merge(SNPs_all, uus_freq, by = "rsid")
SNPs_all$freq <- as.numeric(SNPs_all$freq)

#check that effect allele and ALT allele match up
SNPs_all$chk_alt <- ifelse(SNPs_all$effect_allele==SNPs_all$ALT.1, 1, 0)

#check if UUS frequency matches up with alt_freq or effect allele
SNPs_all$frq_diff <- abs(SNPs_all$ALT_Frq - SNPs_all$freq)
SNPs_all$chk_freq <- ifelse(abs(SNPs_all$ALT_Frq - SNPs_all$freq) < 0.01, 1, 0)

#ignore these checks as now have pre-flipped data.
SNPs_all$rsid_allele <- paste(SNPs_all$rsid,SNPs_all$effect_allele, sep="_", collapse=NULL)

#--------------------------------------------------------------#
# Calculation of PRS - one SNP at a time added from highes to lowest effect size.
#--------------------------------------------------------------#

#order by p-value
SNPs_all<- SNPs_all[ order(SNPs_all$p, decreasing=FALSE), ]

#remove 1 SNP at a time and create score
score<- data.frame(matrix(nrow=10792, ncol=15))
                 
for (i in 1:15){
  #remove row from SNP_effect  
  SNP <- SNPs_all[1:i, ] 
  #---------------#
  # Set up vectors for calculation of PRS
  #---------------#
  #keep rsid and beta
  SNP_effect <- rbind(SNP$rsid_allele, SNP$beta)
  SNP_effect2 <- SNP$beta
  
  #order columns in UUS data to correspond to effects vector
  file2_reorder <- file2[c(SNP_effect[1,1:i])]
  
  #---------------#
  #create score - multiply coefficient by number of alleles
  #---------------#
  PRS_score <- mapply(`*`, file2_reorder, SNP_effect2)
  PRS_score <- as.data.frame(PRS_score)
  
  #sum totals per individual
  PRS_score$score <- (rowSums(PRS_score))
  score[i] <- PRS_score$score
  
  #---------------#
  # add back to UUS data - covariates
  #---------------#
  
  file2 <- cbind(file2, score[i])
}

PRS <- file2
save(PRS, file="./data/UUS_15_SNP_cumulative_GRS.RData")
load("./data/UUS_15_SNP_cumulative_GRS.RData")

PRS$statusc <- factor(PRS$status,labels = c("control", "case"))
PRS$sexc <- factor(PRS$sex,labels = c("female", "male"))

PRSmale <-PRS[PRS$sexc=="male",]
PRSfemale <-PRS[PRS$sexc=="female",]
SNP_ord <- rbind(SNPs_all$rsid_allele, SNPs_all$beta)

#--------------------------------------------------------------#
# AUC of each
#--------------------------------------------------------------#
AUCs<- data.frame(matrix(nrow=15*3, ncol=6))
j<- 1
for (i in 1:15){
  var <-paste0("X",i)
  roca <- roc(PRS$status, PRS[[var]], ci=TRUE)
  rocm <- roc(PRSmale$status, PRSmale[[var]], ci=TRUE)
  rocf <- roc(PRSfemale$status, PRSfemale[[var]], ci=TRUE)
  roc.test(rocm, rocf)
  
  # AUCs[j,1] <- "Overall"
  # AUCs[j+1,1] <- "Male"
  # AUCs[j+2,1] <- "Female"
  AUCs[j,1] <- 1
  AUCs[j+1,1] <- 2
  AUCs[j+2,1] <- 3
  
  AUCs[j,2] <- roca$auc
  AUCs[j+1,2] <- rocm$auc
  AUCs[j+2,2] <- rocf$auc
  
  #CI
  AUCs[j,3] <- roca$ci[1]
  AUCs[j+1,3] <- rocm$ci[1]
  AUCs[j+2,3] <- rocf$ci[1]
  
  AUCs[j,4] <- roca$ci[3]
  AUCs[j+1,4] <- rocm$ci[3]
  AUCs[j+2,4] <- rocf$ci[3]
  
  AUCs[j,5] <- i
  AUCs[j+1,5] <- i
  AUCs[j+2,5] <- i
  
  AUCs[j,6] <- SNP_ord[1,i]
  AUCs[j+1,6] <- SNP_ord[1,i]
  AUCs[j+2,6] <- SNP_ord[1,i]
  j<- j+3
  
}

#name columns
AUCs$X1 <- factor(AUCs$X1,labels = c("Overall", "Male", "Female"))
AUCs <- AUCs[AUCs$X1=="Male"|AUCs$X1=="Female",]


#labels
xlabs <- SNPs_all$rsid
pd <- position_dodge(.6)    ### How much to jitter the points on the plot

ggplot(AUCs,                ### The data frame to use.
       aes(x     = X5,
           y     = X2,
           color = X1)) +
  
  geom_point(shape = 15,
             size  = 2,
             position = pd) +
  
  geom_errorbar(aes(ymin  = X3,
                    ymax  = X4),
                width = 0.2,
                size  = 0.7,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) +
  ylab("AUC") +
  scale_y_continuous(name="AUC", limits=c(0.61, 0.815), n.breaks=6) +
  xlab("SNP Added to Score") +
  scale_x_continuous(name="SNP Added to Score", breaks=seq(1,15,1), labels=xlabs) +
  labs(color = " ") +
  scale_color_manual(values = c("Male"="blue",
                                "Female"="red"))

