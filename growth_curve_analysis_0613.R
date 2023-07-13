# pidSpryC, E. coli DH5alpha ftsz gene, guides 2 & 5-9

# load libraries
library(gcplyr)
library(dplyr)
library(ggplot2)
library(lubridate)

# import data as wide format
wide_data_0613 <- read_wides("061323_ftsZpidSpryC_cleandata.csv")
str(wide_data_0613)
head(wide_data_0613)

# transform to tidy format
tidy_data_0613 <- trans_wide_to_tidy(
  wides = wide_data_0613,
  id_cols = c("file", "Time", "Temp"))
str(tidy_data_0613)
head(tidy_data_0613)

# design info
design_0613 <- make_design(
  output_format = "tidy", # set to "blocks" to check that it's correct, then save as "tidy"
  nrows = 8, ncols = 12, lookup_tbl_start = "A",
  Guide = list( 
    c("empty", "2A", "5", "6", "7", "8", "9", "none"), # values
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
head(design_0613)

# merge data and design data frames
data_design_merged_0613 <- merge_dfs(tidy_data_0613, design_0613)
str(data_design_merged_0613)
head(data_design_merged_0613)

# convert time to numeric
data_design_merged_0613$Time <- time_length(hms(data_design_merged_0613$Time), unit = "hour")
head(data_design_merged_0613)

# reorder Well and Guide levels
data_design_merged_0613$Well <- factor(data_design_merged_0613$Well,
                                       levels=paste(rep(LETTERS[1:8], each=12), 1:12, sep=""))
data_design_merged_0613$Guide <- factor(data_design_merged_0613$Guide,
                                        levels=c("empty", "2A", "5", "6", "7", "8", "9", "none"))
str(data_design_merged_0613)

# plot data
ggplot(data = data_design_merged_0613, aes(x=Time, y=Measurements, color=ATC_uM)) +
  geom_line() +
  facet_wrap(~Well, nrow = 8, ncol = 12)

ggplot(data = data_design_merged_0613, aes(x=Time, y=Measurements, color=ATC_uM)) +
  geom_line() +
  facet_grid(rows=vars(Guide), cols = vars(replicates))

ggplot(data = data_design_merged_0613, aes(x=Time, y=Measurements, color=replicates)) +
  geom_line() +
  facet_grid(rows=vars(Guide), cols = vars(ATC_uM))
