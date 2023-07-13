# walk-through: https://mikeblazanin.github.io/gcplyr/articles/gcplyr.html 

# install packages
#install.packages("gcplyr")
#install.packages("lubridate")

# load libraries
library(gcplyr)
library(dplyr)
library(ggplot2)
library(lubridate)

# practice with previous dataset 05/31/23

# import data
# save data in csv format
# data is wide-shaped (each row is a timepoint, each column is a well from the pate)
# use: read_wides
imported_widedata_0531 <- read_wides("ftsZpidSpryC_growthCurves_05312023_cleanData.csv")
str(imported_widedata_0531)

# transform to tidy-shaped (each unique measurement has its own row)
# use trans_wide_to_tidy, specify wide data frame and which columns are not data
tidy_data_0531 <- trans_wide_to_tidy(
  wides = imported_widedata_0531,
  id_cols = c("file", "Time"))
str(tidy_data_0531)
head(tidy_data_0531)

# import blank data (used in analyzing lag time and max growth rate)
blank_widedata_0531 <- read_wides("ftsZpidSpryC_growthCurves_05312023_blankData.csv")
str(blank_widedata_0531)
blank_tidydata_0531 <- trans_wide_to_tidy(
  wides = blank_widedata_0531,
  id_cols = c("file", "Time")
)

# drop 'file' column and rename 'Measurements' column so it can be merged later
blank_tidydata_0531 <- blank_tidydata_0531 %>%
  select(-file) %>%
  rename(Blank = Measurements)
str(blank_tidydata_0531)

# incorporate design info
design_0531 <- make_design(
  output_format = "tidy", # set to "blocks" to check that it's correct, then save as "tidy"
  nrows = 8, ncols = 12, lookup_tbl_start = "A",
  Guide = list( 
    c("empty", "1A", "1B", "2A", "3A", "3B", "4C", "4D"), # values
    1:8, # rows to apply to (those left out will be filled in with NA)
    1:12, # columns to apply to
    "ABCDEFGH", # pattern of the values
    FALSE # indicate whether filled by row (TRUE) or by column (FALSE)
  ), # set design for guide
  ATC_uM = list( 
    c(0,0,0,0.1,0.1,0.1,0.4,0.4,0.4,1,1,1),
    1:8,
    1:12,
    "ABCDEFGHIJKL",
    TRUE
  ), # set design for ATC concentration
  replicates = list(
    c(1,2,3,1,2,3,1,2,3,1,2,3),
    1:8,
    1:12,
    "ABCDEFGHIJKL",
    TRUE
  )
)
design_0531
str(design_0531)

# merge data and design data frames
data_design_merged_0531 <- merge_dfs(tidy_data_0531, design_0531)
str(data_design_merged_0531)
head(data_design_merged_0531)

# merge blank data
data_design_merged_0531 <- merge_dfs(data_design_merged_0531, blank_tidydata_0531)

# exclude data if needed (contamination, negative controls, etc)

# convert time from character string to numeric
# hms() since data has hours, minutes, and seconds (use hm() for hours & minutes)
# set unit to be hours (defaults to seconds)
data_design_merged_0531$Time <- time_length(hms(data_design_merged_0531$Time), unit = "hour")
head(data_design_merged_0531)

# reorder Well levels so that they plot in the right order
# this part generates A1-H12: paste(rep(LETTERS[1:8], each=12), 1:12, sep="")
data_design_merged_0531$Well <- factor(data_design_merged_0531$Well,
                                       levels=paste(rep(LETTERS[1:8], each=12), 1:12, sep=""))

# reorder Guide levels
data_design_merged_0531$Guide <- factor(data_design_merged_0531$Guide,
                                        levels=c("empty", "1A", "1B", "2A", "3A", "3B", "4C", "4D"))
str(data_design_merged_0531)

# plot the data
ggplot(data = data_design_merged_0531, aes(x=Time, y=Measurements, color=ATC_uM)) +
  geom_line() +
  facet_wrap(~Well, nrow = 8, ncol = 12)

ggplot(data = data_design_merged_0531, aes(x=Time, y=Measurements, color=ATC_uM)) +
  geom_line() +
  facet_grid(rows=vars(Guide), cols = vars(replicates))

ggplot(data = data_design_merged_0531, aes(x=Time, y=Measurements, color=replicates)) +
  geom_line() +
  facet_grid(rows=vars(Guide), cols = vars(ATC_uM))

# analyze data
# calculate per capita derivative
data_design_merged_0531 <-  data_design_merged_0531 %>%
  group_by(Well, Guide, ATC_uM, replicates) %>%
  mutate(deriv_percap = calc_deriv(x=Time, y=Measurements, percapita = TRUE, blank = 0))

ggplot(data=data_design_merged_0531, aes(x=Time, y=deriv_percap)) +
  geom_line() +
  facet_wrap(~Well, scales="free")
 
# calculate lag time, max growth rate, area under curve (total growth)
data_0531_sum <- data_design_merged_0531 %>%
  group_by(Well, Guide, ATC_uM, replicates) %>%
  summarize(min_dens = first_minima(Measurements, return="y"),
    lag_time = lag_time(y=Measurements, x=Time, deriv = deriv_percap, y0 = min_dens),
    max_growth_rate = max_gc(deriv_percap, na.rm=T),
    time_max_growth = extr_val(Time, which_max_gc(deriv_percap)),
    area_und)

ggplot(data=data_design_merged_0531, aes(x=Time, y=deriv_percap)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_point(data=data_0531_sum, aes(x=time_max_growth, y=max_growth_rate),
             size = 2, color="red") +
  coord_cartesian(ylim=c(-1, NA))




