## Read data from Campbell Sci logger files.  
##  
## For table loggers, i.e. TOA5 files
## column names are in the 2nd row of the file
## For MIXED ARRAY DATA FILES**(Older array data without variable names.)
## The function importCSdata expects the .fsl file in the same directory
## with the same name as the data file but with .fsl extension
## The .fsl file has the column labels. A list of dataframes will
## be returned. The lists names will be the ID numbers.
## Jim Laundre 2024-04-01
## Revised

# REQUIRED PACKAGES ------------------------------------------------------
packages <- c("ggplot2","ggtext","janitor","lubridate",
              "plotly","readxl","stringr","tidyverse", "viridis","rstudioapi")

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Functions --------------------------------------------------------------

source("./R/importCSdata.r")

##  get the list of .dat logger files in a directory

get_dat_files <- function(x){ 
  proc_path <- rstudioapi::selectDirectory(caption = "Select .dat logger files Directory")
  # Get the list of filenames of from directory (recursive=F) without path (full.names=F)
  x <- list.files(proc_path, full.names= T, pattern='*.dat$', recursive=FALSE) %>% 
    setNames(nm= basename(.)) # set names for list
  return(x)
}

get_current_files <-function(site){
  proc_path <- rstudioapi::selectDirectory(caption = "Select .dat logger files Directory")
  # Get the list of filenames of from directory (recursive=F) without path (full.names=F)
  file_pattern <- paste0("^",site,".*\\.dat$")
  list.files(proc_path, full.names= T, pattern=file_pattern, recursive=FALSE) %>% 
    setNames(nm= basename(.)) # set names for list  
  return()
}

# Process the .dat files ---------------------------------------------------

# Select the directory of the logger .dat files and list the *.dat$ files  
logger_files <- get_dat_files()

# Get a csv file that maps old to new variable names
variable_file <- rstudioapi::selectFile(caption = "Select variable names csv file",
                                        path =dirname(logger_files[1]), 
                                        filter = "CSV files (*.csv)")
var_names <- read_csv(variable_file, col_names=T, show_col_types = FALSE )

station <- sub(pattern = "(^[^_]+)(_.*)", replacement = "\\1", basename(variable_file))
# site<- str_split_1(station,"_") 
# site <- paste0(site[1],"_",site[2])
# current_files <-get_current_files(site)
# Read all the files into a tibble. Rename the columns. Note: any columns not in the variable_file
#  will not be renamed.  


logger_data <- logger_files %>%
  map(function(x) {
    df_logger_data <- importCSdata(x) %>%
     # {if(is.list(.))flatten(.)}  %>% 
    
      # remove exact duplicates
      distinct()  
    # rename columns to standard names using the newname column of the var_names csv file
    name_match = match(tolower(var_names$var), names(df_logger_data))
    names(df_logger_data)[na.omit(name_match)] = var_names$newnames[!is.na(name_match)]
    df_logger_data
  }) %>%
  # Add the .dat file names as a column
  list_rbind(names_to = "file_name") %>%
  arrange(timestamp) %>%
  force_tz("Etc/GMT+9") %>%
  select(file_name, timestamp, everything())

# Check the column names
names(logger_data)

# Calculate full time series to fill for missing times ----

data_time <- unique(logger_data$timestamp)
# Get the time difference between timestamps to get the collection interval.
data_interval <-
  difftime(data_time[-1], data_time[-length(data_time)], unit = "mins")
# Use the first interval as the time step.
# !! Will not work if the collection interval changes between data files
date_range <-
  as_tibble_col(seq.POSIXt(min(logger_data$timestamp), max(logger_data$timestamp), by =
                             data_interval[1]), column_name = "timestampjoin")

#Join the full range of dates ----
df_logger <- logger_data %>%
  mutate(timestampjoin = timestamp) %>%
  full_join(., date_range) %>%
  select(-timestamp) %>% 
  select(file_name, timestamp = timestampjoin, contains("Air"), contains("RH"), everything()) %>%
  .[order(.$timestamp), ]

# Check for duplicate timestamps. If any, review them.
dups <- get_dupes(df_logger, timestamp)
view(dups)
message("Review the dup dataframe and choose the appropitate removal script.")

# Combine duplicate timestamp rows where the data are not all in the same .dat file
# Grouped by timestamp than use tidyr fill to fill in the missing values.  Slice removes the duplicates
df_logger <- df_logger %>%
  group_by(timestamp) %>%
  fill(everything(), .direction = "downup") %>%
  slice(1) %>% 
  select(-file_name)

# Calculate soil moisture using calibration equation from Joe's REU project----
# Equation: % soil moisture = (measure vw * 100) * 0.61 + 7.2
# round to 1 decimal place

cal_sm <- function(sm_fractional){
  round(
    (sm_fractional*100) * 0.61 + 7.2 
  ,digits = 1)
}

df_logger<- df_logger %>%  
  mutate(across(contains("_VW"),
                .fn = cal_sm,
                .names = "{.col}_percent"
                )
         )

# Statements for filtering out bad values ------------------------------------

# # Blk3 air had faulty air temp wire in 2023 
# df_logger <- df_logger %>%
#   mutate(CT_Air_3M_Avg = ifelse(
#     year(timestamp) == 2023 &
#      (month(timestamp) == 7 &
#        CT_Air_3M_Avg < -39 |CT_Air_3M_Avg == -27.37),
#     NA,
#     CT_Air_3M_Avg
#   )
  # ,CT_RH = ifelse(
  #   year(timestamp) == 2023 &
  #     (month(timestamp) == 7 &
  #     CT_RH < 0),
  #     NA,
  #     CT_RH
  #     )
  #)


# remove -7999 which is Campbell's missing data value
  df_logger <- df_logger %>% mutate(across(where(is.numeric), ~na_if(.,-7999))) 

#Save data using station name. Station name is taken from the csv variable_file name ----
#station <- sub(pattern = "(.*)(_var_names)\\..*$", replacement = "\\1", basename(variable_file))
range_data <- paste0(date(min(df_logger$timestamp)),"_",date(max(df_logger$timestamp)))
write_rds(df_logger, file = paste0(dirname(variable_file),"/df_", station, "_", range_data,".rds"))
write.csv(df_logger,
          file = paste0(dirname(variable_file),"/",station, "_", range_data,".csv"),
          row.names = F)


# Plot data --------------------------------------------------------------------
# 
# Load data file if already combined.
 df_logger <- read_rds(file =  rstudioapi::selectFile(caption = "Select variable names rds file",
                                       filter = "rds files (*.rds)"))
# _______________________________________________________________________________

p1 <-  ggplot(df_logger %>%
               select(timestamp, contains("air")) %>%    # Reverse order of key so CT is on top                                            
               pivot_longer(-timestamp, names_to = "key", names_transform = list(key = fct_rev), 
                            values_to = "value", values_drop_na = TRUE)) +
  geom_line(aes(x=timestamp,  y = value, color = key)) +
  geom_hline(aes(yintercept = 0))+
  scale_x_datetime(expand = expansion(mult = c(.01, .01))) +
  scale_color_manual(values = c("red","blue","orange")) +
  labs(title = "Met Data",
       subtitle = "LTER plots",
       x = "Date",
       y = "Degrees Celsius",
       color = '') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.7),
        legend.position = "top")
p2 <- ggplot(df_logger %>%
               select(timestamp, contains("rh")) %>%       # Reverse order of key so CT is on top                                             
               pivot_longer(-timestamp, names_to = "key", names_transform = list(key = fct_rev),
                            values_to = "value", values_drop_na = TRUE)) +
  geom_line(aes(x=timestamp,  y = value, color = key, group = desc(key))) +
  scale_color_manual(values = c("red","blue")) +
  labs(title = "Air RH",
       x = "Date",
       y = "Relative Humidity (%)",
       color = '')+
  coord_cartesian(ylim = c(NA,100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.7),
        axis.title.y = element_markdown(color = "black", size = 8))
p3 <- ggplot(df_logger%>%
               select(timestamp, contains("batt")) %>%                                                
               pivot_longer(-timestamp, names_to = "key", values_to = "value", values_drop_na = TRUE)
             ) +
  geom_line(aes(x=timestamp,  y = value, color = "Battery avg")) +
  scale_x_datetime()+
  labs(title ="Battery",
       x = "Date",
       y = "Volts",
       color = '')+
  coord_cartesian(ylim = c(8,15)) +
  scale_color_manual(values = c("Battery avg" = "black"))+
  theme_bw() 


# set the min and max for the initial x axis display in ggplotly
max_date <- max(df_logger$timestamp)
min_date <- max_date - lubridate::days(15) 

# ---- Setting up interactive plots with ggplotly-------
# Note for rangeslider to work:
# the dynamicTicks needs to be true for the buttons to show
# autorange needs to be FALSE for range to work

# Define xaxis options for using a range slider
xax<- list( 
  autorange=F,
  range= list(min_date, max_date),
  rangeselector = list(
    buttons = list(
      list(count = 1, label = "1 week", strp ="week", stepmode = "backward"),
      list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
      list(count = 6, label = "6 mo", step = "month", stepmode = "backward"),
      list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
      list(step = "all")
    )),
  rangeslider = list(type = "date", thickness=0.05))
# define zoom for y axis
yax <- list(fixedrange = FALSE)


# Create a list of arguments for the annotation layout to add titles to the subplots
anno_agr <-list(x = .5,
                text = "",
                y = 1,
                yref = "paper",
                xref = "paper",
                xanchor = "center",
                yanchor = "top",
                yshift = 20,
                showarrow = FALSE,
                font = list(size = 15))

# Air Temperature panel ---------------------------------------------------
# First convert ggplot plots to ggplotly

#Title plot 1
anno_agr$text <- "Air Temperature"
p1_p <- ggplotly(p1,dynamicTicks = T) %>% 
  layout(xaxis= xax,
         yaxis =list(yax,zerolinewidth = .1),
         annotations = anno_agr) %>% 
  partial_bundle()

#Title plot 2
anno_agr$text <- "Relative Humidty"
p2_p <- ggplotly(p2,dynamicTicks = T) %>%
  layout(xaxis= xax,
         annotations = anno_agr) %>%
  partial_bundle()

#Title plot 3
anno_agr$text <- "Battery"
p3_p <- ggplotly(p3,dynamicTicks = T) %>% 
  layout(xaxis= list( 
    autorange=F,range= list(min_date, max_date)),
    yaxis = list(autorange = FALSE),
    annotations = anno_agr) %>% 
  partial_bundle()

#Put the plots together on a panel 
panel1 <- subplot(p2_p,p1_p,p3_p, nrows=3, shareX = TRUE,titleY = T,heights = c(.3,.5,.2),which_layout = 2)
panel1
htmlwidgets::saveWidget(panel1, paste0(dirname(variable_file),"/",str_remove(station,"_soil"),"_", range_data,".html"),title = "MAT06 Met")


#Soil Plots for CT, GH  -----------------------------------

soil_data <- df_logger
pal1<- viridis(12)[c(10:12,6:8)]
sp1 <- soil_data %>% select(timestamp, contains("avg")  & -contains("battery")) %>%                                                
  pivot_longer(-timestamp, names_to = "key", values_to = "value", values_drop_na = TRUE) %>%
  ggplot(data=.,aes(x=timestamp, y = value,color=key)) +
  scale_x_datetime()+
  coord_cartesian(ylim = c(-15,15)) +
  labs(title ="Soil Temperatures and Moisture",
       x = "Date",
       y = "Celsius ",
       color = 'Legend')+
  #scale_color_manual(values = pal1)+
  theme_bw() +
  geom_line(aes(color = key),linewidth = .3) +
  theme(
    plot.title = element_text(hjust = 0.7)
  )
# 
# sp2 <- soil_data %>% select(timestamp,intersect(contains("GH"), contains("10cm_Avg_C"))) %>%
#   gather("key", "value", -timestamp) %>%
#   mutate(treatment =as.factor(str_extract(key,"[GHghCTct]+"))) %>%
#   ggplot(data=., aes(x=timestamp, y = value, color=key)) +
#   scale_x_datetime()+
#   labs(title ="Soil Temperatures",
#        x = "Date",
#        y = "Celsius ",
#        color = 'Legend')+
#   coord_cartesian(ylim = c(NA,15)) +
#   theme_bw() +
#   geom_line(aes(color = key, linewidth = treatment)) +
#   guides(linewidth = "none") +
#   scale_linewidth_manual(values = c("GH" = .1, "CT" = 0.2))

sp3 <- soil_data %>% select(timestamp,contains("vw_percent")) %>%
  mutate(across(where(is.numeric), ~na_if(.,-7999))) %>%
  gather("key", "value", -timestamp) %>%
  mutate(treatment =str_extract(key,"[GHghCTct]+")) %>% 
  drop_na() %>% 
  ggplot(data=., aes(x=timestamp, y = value, color = key)) +
  scale_x_datetime()+
  labs(x = "Date",
       y = "Percent(VW)",
       color = 'Legend')+
  coord_cartesian(ylim = c(0,NA)) +
  scale_color_manual(values = pal1)+
  theme_bw() +
  geom_line(aes(color = key),linewidth = .3) 

# Soil Temperature interactive panel ----
# First convert ggplot plots to ggplotly
#Title plot 1 
anno_agr$text <- "Plot Soil Temperatures"  
sp1_p <- ggplotly(sp1,dynamicTicks = T) %>% 
  layout(xaxis= xax,
         yaxis = yax,
         annotations = anno_agr
  ) %>% 
  partial_bundle()

#Title plot 2
# anno_agr$text <- "F10 Plot Soil Temperatures"
# sp4_p <- ggplotly(sp2,dynamicTicks = T) %>% 
#   layout(xaxis= xax,
#          yaxis = list(autorange=F,
#                       fixedrange= FALSE),
#          annotations = anno_agr
#   ) %>% 
#   partial_bundle()

#Title plot 3
anno_agr$text <- "Plot Soil moisture (VW)"
sp3_p <- ggplotly(sp3,dynamicTicks = T) %>% 
  layout(xaxis = xax,
         yaxis = list(autorange=F,
                      fixedrange= FALSE),
         annotations = anno_agr) %>% 
  partial_bundle()
panel2 <- subplot(sp1_p,sp3_p, nrows=2, shareX = TRUE,titleY = T)
panel2
htmlwidgets::saveWidget(panel2,paste0(dirname(variable_file),"/", str_remove(station,"_met"), "_", range_data, ".html"), title = "Plot Soil")

