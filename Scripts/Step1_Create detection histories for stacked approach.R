library(unmarked)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(MuMIn)

# Step One: Create detection histories for each species

padop = read.csv("~/Dropbox/Billy/_research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Predator data/sandpads.active.long.csv")

lookup = read.csv("~/Dropbox/Billy/_research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Predator data/sandpads.lookup.csv")

padop$Start = as.Date(padop$Start, format = "%d/%m/%y")
padop$End = as.Date(padop$End, format = "%d/%m/%y")
padop$Problem1_start = as.Date(padop$Problem1_start, format = "%d/%m/%y")
padop$Problem1_end = as.Date(padop$Problem1_end, format = "%d/%m/%y")
padop$Problem2_start = as.Date(padop$Problem2_start, format = "%d/%m/%y")
padop$Problem2_end = as.Date(padop$Problem2_end, format = "%d/%m/%y")

detection.data = read.csv("~/Dropbox/Billy/_research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Predator data/SandpadRawData_BGeary_200508MM_rawdata.csv")

data = detection.data %>% 
  mutate(Date = as.Date(Date, format = "%d-%b-%y")) %>%
  filter(PrintCondGauge == TRUE) # Select only data points with good print condition


# Get the sand pads and dates that the condition guage is no good (treat as NAs)
bad.cond = detection.data %>% 
  mutate(Date = as.Date(Date, format = "%d-%b-%y")) %>%
  filter(PrintCondGauge == FALSE) %>%
  select(SandPadID, Date) %>%
  mutate(Cond = "Bad") %>%
  mutate(Site = as.factor(SandPadID))

sessions = unique(padop$Session) 
session.list = list()
full.sesh= data.frame()

for (s in 1:length(sessions)){
  sesh = sessions[s]
  padop.sesh = filter(padop, Session == sesh) %>% drop_na(Start, End)
  transects = unique(padop.sesh$Transect)
  
  for (t in 1:length(transects)){
    trans = transects[t]
    trans.sesh = filter(padop.sesh, Transect == trans) %>% rowwise() %>%
      do(data.frame(Transect=.$Transect, Date=seq(.$Start,.$End,by="1 day")))
    trans.sesh$Session = paste(sesh)
    full.sesh = rbind(full.sesh, trans.sesh)
  }
  session.list[[sesh]] = full.sesh
  full.sesh = data.frame()
}

effort = do.call(rbind.data.frame, session.list)

problems1 = padop %>% drop_na(Problem1_start) %>% rowwise() %>% 
  do(data.frame(Transect=.$Transect, Date=seq(.$Problem1_start,.$Problem1_end,by="1 day")))
problems2 = padop %>% drop_na(Problem2_start) %>% rowwise() %>% 
  do(data.frame(Transect=.$Transect, Date=seq(.$Problem2_start,.$Problem2_end,by="1 day")))
problems = rbind(problems1, problems2)

problems$Problem = "Problem"

effort = left_join(effort, problems, by=c("Transect", "Date"))

effort = effort %>% filter(is.na(Problem)) %>% dplyr::select(-Problem) %>% group_by(Transect, Session) %>% 
  mutate(Occ = row_number())

effort$Session = as.numeric(effort$Session)

Occ = 1:12  
Sites = lookup$Site
Session = as.numeric(1:21)
occ.lookup = expand.grid(Sites, Session, Occ)
colnames(occ.lookup) = c("Site","Session", "Occ")
lookup = left_join(lookup, occ.lookup)

effort = left_join(lookup, effort, by=c("Transect", "Session", "Occ"))

## add in the bad print condition guages
effort = left_join(effort, bad.cond, by=c("Site", "Date"))
effort$Date[effort$Cond=='Bad'] <- NA
effort = effort %>% select(-SandPadID, -Cond)


full.data = left_join(effort, data, by=c("Site"="SandPadID", "Date"))

full.data$Year = as.numeric(year(full.data$Date)) - 2005

session.check = full.data %>% group_by(Session, Transect, Date, Year) %>% summarise()

# Create Fox detection history

full.data$Fox = ifelse(is.na(full.data$Fox), 0, full.data$Fox)
full.data$Fox = ifelse(full.data$Fox >1, 0, full.data$Fox) # Use only the certain data (Certain = 1), convert uncertain to absence
full.data$Fox = ifelse(is.na(full.data$Date), NA, full.data$Fox)

foxes.long = full.data %>% dplyr::select(Session, Site, Occ, Fox) %>% mutate(Fox = as.numeric(Fox))

foxes.dethist = foxes.long %>% pivot_wider(id_cols = c(Site,Session), names_from = Occ, values_from = Fox) %>% as.matrix()

row.names(foxes.dethist) = paste0(foxes.dethist[,1],"_S",foxes.dethist[,2])

save(foxes.dethist, file = "~/Dropbox/_research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Predator data/foxes.sandpads.dethist.21052021.Rda")

# Create Cat detection history
full.data$Cat = ifelse(is.na(full.data$Cat), 0, full.data$Cat)
full.data$Cat = ifelse(full.data$Cat >1, 0, full.data$Cat) # Use only the certain data (Certain = 1), convert uncertain to absence
full.data$Cat = ifelse(is.na(full.data$Date), NA, full.data$Cat)

cats.long = full.data %>% dplyr::select(Session, Site, Occ, Cat) %>% mutate(Cat = as.numeric(Cat))
cats.dethist = cats.long %>% pivot_wider(id_cols = c(Site,Session), names_from = Occ, values_from = Cat) %>% as.matrix()

row.names(cats.dethist) = paste0(cats.dethist[,1],"_S",cats.dethist[,2])

save(cats.dethist, file="~/Dropbox/_research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Predator data/cats.sandpads.dethist.21052021.Rda")

