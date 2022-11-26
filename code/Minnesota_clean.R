library(readr)

## MINNESOTA PANEL ##
path <- "/Users/research/Desktop/valeska-honors-new/"
panel_data <- mixedsort(sort(list.files(paste0(path, "data-clean/"), "panel.*\\.dta$", full.names = FALSE)))
rbind()
p <- read_dta(paste0(path,"data-clean/panel_2015.dta")) 
w <- read_dta(paste0(path,"data-clean/panel_2016.dta"))
a <- read_dta(paste0(path,"data-clean/panel_2017.dta"))
b <- read_dta(paste0(path,"data-clean/panel_2018.dta"))
c <- read_dta(paste0(path,"data-clean/panel_2019.dta"))
d <- read_dta(paste0(path,"data-clean/panel_2020.dta"))
z <- rbind(p,w,a,b,c,d)

#for(j in 1:length(panel_data)){
  p <- read_dta(paste0(path,"data-clean/", x))
  w <- read_dta(paste0(path,"data-clean/", x))
  z <- rbind(all)
#} ## ask how i'd do the loop

z <-transform(z, state = as.numeric(STATEFP))
mn_panel <- z %>% 
  filter(state == 27)
# Write to CSV file
write_csv(mn_panel, "/Users/research/Desktop/valeska-honors-new/data-clean/mn_panel.csv")

