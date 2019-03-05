library(dplyr)
library(lubridate)
library(xlsx)

setwd("Q:/MultiCare Inpatient Specialists/MIS Manager/Jack MIS Manager/Analysis & Reporting/Performance Summary/Index")

# load DRG identities
medical.drgs <- read.csv("bpci.drg.csv")
sepsis.drg <- c(870,871,872)
hf.drg <- c(291,292,293)
copd.drg <- c(190,191,192,202,203)
pneumonia.drg <- c(171,178,179,193,194,195)
stroke.drg <- c(61,62,63,64,65,66)
renal.drg <- c(682,683,684)
uti.drg <- c(689,690)
gi.hem.drg <- c(377,388,389)
cell.drg <- c(602,603)
gi.obs <- c(388,389,390)
gi.major.bowel <- c(329,330,331)


# time frames
this.month <- mdy("12-31-18")
one.month <- mdy("12-1-18")
three.months <- mdy("10-1-18")
six.months <- mdy("7-1-18")
twelve.months <- mdy("1-1-18")

# read in MIS ID #s
med.staff <- read.xlsx("Epic IDs.xlsx", sheetIndex = "All")
physician.ID1 <- med.staff$EPIC.IDs.2
physician.ID2 <- med.staff$EPIC.ID

# read in utilization data
hist.data <- read.csv("inpatienthistorical.csv")
hist.data$Dischg.Date <- mdy(hist.data$Dischg.Date)
hist.data$Total.Costs <- gsub("[^0-9.]","",hist.data$Total.Costs)
hist.data$Net.Margin <- gsub("[^0-9.]","",hist.data$Net.Margin)
hist.data$NetRev <- gsub("[^0-9.]","",hist.data$NetRev)
hist.data$Direct <- gsub("[^0-9.]","",hist.data$Direct)

mis.data <- hist.data %>% filter(Attending.ID %in% physician.ID2)
bpci.data <- hist.data %>% filter(DRG %in% medical.drgs$drg & Age >17)


# read in readmissions
readmissions <- read.csv("12 month readmission data.csv")
readmissions$Discharge.Date <- mdy(readmissions$Discharge.Date)
mis.readmissions <- readmissions %>% filter(Discharge.Provider.ID %in% physician.ID1)
bpci.readmissions <- readmissions %>% filter(MSDRG.Code %in% medical.drgs$drg)

## Hospital Summary
hospital.summary <- hist.data  %>% group_by(Facility) %>% summarise(cmi = mean(MSDRG.Weight),
                                                                    los = mean(LOS),
                                                                    mortality = sum(grepl("Expired",Disch.Status)),
                                                                    discharges = length(HAR)) %>%
                                   mutate(adjusted.los = los / cmi,
                                          mortality.rate = mortality / discharges,
                                          adjusted.mortality = mortality.rate / cmi)

# Expense Summary
trailing.nine <- hist.data %>% filter(Dischg.Date %in% three.months:twelve.months) %>% group_by(Facility) %>%
              summarise(cmi = mean(MSDRG.Weight),
                        expense = mean(as.numeric(as.character(Total.Costs)))) %>%
              mutate(adjusted.expense = expense / cmi)


# Readmissions
readmission.rate <- readmissions %>% group_by(Discharge.Facility) %>% summarise(readmits = sum(Readmissions..with.Exclusions.),
                                                                      discharges = sum(Discharges..with.Exclusions.)) %>%
                    mutate(readmit.rate = readmits / discharges)



## MIS Performance Summary
mis.summary <- mis.data  %>% group_by(Facility) %>% summarise(cmi = mean(MSDRG.Weight),
                                                                    los = mean(LOS),
                                                                    mortality = sum(grepl("Expired",Disch.Status)),
                                                                    discharges = length(HAR)) %>%
  mutate(adjusted.los = los / cmi,
         mortality.rate = mortality / discharges,
         adjusted.mortality = mortality.rate / cmi)

# Expense Summary
mis.trailing.nine <- mis.data %>% filter(Dischg.Date %in% three.months:twelve.months) %>% group_by(Facility) %>%
  summarise(cmi = mean(MSDRG.Weight),
            expense = mean(as.numeric(as.character(Total.Costs)))) %>%
  mutate(adjusted.expense = expense / cmi)


# Readmissions
mis.readmission.rate <- mis.readmissions %>% group_by(Discharge.Facility) %>% summarise(readmits = sum(Readmissions..with.Exclusions.),
                                                                                discharges = sum(Discharges..with.Exclusions.)) %>%
                                    mutate(readmit.rate = readmits / discharges)


##### bpci data
bpci.performance.data <- bpci.data %>% group_by(Facility) %>%
                                              summarise(cmi = mean(MSDRG.Weight),
                                                        los = mean(LOS),
                                                        mortality = sum(grepl("Expired",Disch.Status)),
                                                        discharges = length(HAR)) %>%     
  
                                              mutate(adjusted.los = los / cmi,
                                                     mortality.rate = mortality / discharges,
                                                     adjusted.mortality = mortality.rate / cmi)
# bpci expense
bpci.trailing.nine <- bpci.data %>% filter(Dischg.Date %in% three.months:twelve.months) %>% 
                                              group_by(Facility) %>%
                                              summarise(cmi = mean(MSDRG.Weight),
                                                        expense = mean(as.numeric(as.character(Total.Costs)))) %>%
                                              mutate(adjusted.expense = expense / cmi)

# bpci readmissions
bpci.readmission.rate <- bpci.readmissions %>% group_by(Discharge.Facility) %>% summarise(readmits = sum(Readmissions..with.Exclusions.),
                                                                                        discharges = sum(Discharges..with.Exclusions.)) %>%
                                                            mutate(readmit.rate = readmits / discharges)



