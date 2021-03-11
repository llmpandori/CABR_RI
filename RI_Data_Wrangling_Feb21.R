#################################################
# Title: Cleaning Script for RI Data
# Purpose: clean up data for consistency in reporting
# Author: LP
# Last edited: 3/3/2021
##################################################

# next items: 
  # plot summary data (time series for tgt spp in target/transect) in dif ways
  # dig into community/diversity indices

##### packages #####
library(readxl)
library(tidyverse)
library(janitor)
library(wesanderson)
library(PNWColors)
library(ggdark)
library(Hmisc)
library(calecopal)

##### load data #####

# owl limpet data (doesn't need alignment - spp consistent throughout)
# lim_count <- read_excel('Raw Data/MEDN_RI_DB_pre2020/qsummarizer_LIM_count.xlsx')
# lim_density <- read_excel('Raw Data/MEDN_RI_DB_pre2020/qsummarizer_LIM_density_bysize.xlsx')
# lim_measure <- read_excel('Raw Data/MEDN_RI_DB_pre2020/qsummarizer_LIM_measure.xlsx')

# transect data (doesn't need alignment - spp consistent throughout)
phy_transect <- read_excel('Raw Data/MEDN_RI_DB_pre2020/qsummarizer_PHY_bytransect.xlsx')
phy_avg <- read_excel('Raw Data/MEDN_RI_DB_pre2020/qsummarizer_PHY_totalavg.xlsx')

# target data (needs alignment - spp lists differ by year)
target <- read_excel('Raw Data/MEDN_RI_DB_pre2020/qsummarizer_TGT.xlsx')
target_raw <- read_excel('Raw Data/MEDN_RI_DB_pre2020/qsummarizer_TGT_SppN_rawdata.xlsx')

# alignment info for target data 
align <- read_csv('Raw Data/TGT_all.csv')
  # see RI_Species_Alighment_Feb21.R for methods
  # summary: took species lists for 1990 and 2000, applied to all unique spp.

##### tidy: limpet #####
# note to self - get limpet measurements by plot to facilitate merge w/ density

##### tidy: transect ######
# Tidying for consistency comes from B. Hong's 2019-2021 Annual Report Code

# don't need phy_avg summary - adapted from phy_transect data
remove(phy_avg)

# tidy 
transect <- phy_transect %>%
    # remove duplicated rows (not present as of SP20 data)
    distinct() %>%
    # collapse PHYOVE (phyllospadix overstory) and PHYUND (understory) to PHYALL
    mutate(
      SpeciesCode = if_else(SpeciesCode %in% c('PHYOVE', 'PHYUND'), 'PHYALL', SpeciesCode),
      Scientific_name = if_else(Scientific_name %in% c('Phyllospadix spp', 'Phyllospadix torreyi (understory)'), 'Phyllospadix spp.', Scientific_name)) %>%
    # capitalize first letter of strings
    mutate(Scientific_name = str_to_sentence(Scientific_name)) %>%
    # calculate new totals/% cover with new category
    group_by(SiteCode, SiteName, SurveyYear, Season, SeasonName, SurveyDate, Transect, SpeciesCode, Scientific_name, TotalPoints) %>%
    summarise(points = sum(N)) %>%
    # calculate new % cover
    mutate(pct_cover = (points/TotalPoints)*100)
    
remove(phy_transect)

##### tidy: photoplot #####
# Tidying for consistency adapted from B. Hong's 2019-2021 Annual Report Code

# don't need target (doesn't have plot-level info)
remove(target)
  
# keep target raw, retains plot-level data, and tidy
target <- 
  # add generic codes to this set, along with formatted scientific names
  left_join(target_raw, align, by = c('SpeciesCode')) %>%
  # get rid of old scientific name (align has better one)
  select(-c(Scientific_name.x)) %>%
  # rename scientific name column
  rename(Scientific_name = Scientific_name.y) %>%
  # only get unique rows (removes duplicates)
  distinct() 

# summarize by 3 different methods: all categories, 1990 (1990-present), and 2000 (2000-present)

# make list of all possible spp names and codes from target dataset
spplist <- align %>%
  select(SpeciesCode, Scientific_name) %>%
  distinct()

# all categories
target2020 <- target %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name) %>%
  summarise(cover = sum(N))
  

# get total # of pts surveyed per plot in each site/spp/year/season (range = 89-104)
target_summary <- target2020 %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code) %>% 
  summarise(points = sum(cover))

# merge w target dataset (to calculate % cover)
target2020 <- 
  # align # pts per plot with target 
  left_join(target2020, target_summary) %>%
  # calculate % cover (range of # pts is 89 - 104)
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name) %>%
  mutate(pct_cover = (cover/points)*100) %>%
  # streamline dataset
  select(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name, pct_cover) 

# repeat process for 1990
# all categories
target1990 <- target %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_1990) %>%
  summarise(cover = sum(N))

target1990 <- left_join(target1990, target_summary) %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_1990) %>%
  mutate(pct_cover = (cover/points)*100) %>%
  select(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_1990, pct_cover) %>%
  rename(SpeciesCode = Code_1990)

# join spp names to 1990 data
target1990 <- left_join(target1990, spplist)

# repeat process for 2000
target2000 <- target %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_2000) %>%
  summarise(cover = sum(N))

target2000 <- left_join(target2000, target_summary) %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_2000) %>%
  mutate(pct_cover = (cover/points)*100) %>%
  select(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_2000, pct_cover) %>%
  rename(SpeciesCode = Code_2000) %>%
  # limit to applicable years (2000 onward)
  filter(SurveyYear >= 2000) 

target2000 <- left_join(target2000, spplist)


# remove extras
remove(target_summary, target_raw, align, spplist)

##### qc vis set-up #####

# lists of commonly filtered-for items
cabrsites <- c('CAB1', 'CAB2', 'CAB3') 
zonelist <- c('CHT', unique(target$Zone)[1:4])
zonenames <- c('Balanus/Chthamalus', 'Tetraclita rubescens', 'Mussel', 'Silvetia compressa', 'Pollicipes polymerus')
targetlist <- c('CHTBAL', 'TETRUB', 'MUSSEL', 'SILCOM', 'POLPOL')
# theme arguments
lltheme <- theme(text = element_text(size = 12),
                 # add more space between panels
                 panel.spacing = unit(1, 'lines'),
                 # no background to wrap panels
                 strip.background = element_blank(),
                 strip.text = element_text(size = 12),
                 # panel labels outside x axis labels
                 strip.placement = 'outside',
                 # adjust x axis labels
                 axis.text.y = element_text(size = 12),
                 axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

bigtexttheme <- theme(text = element_text(size = 14),
                 # add more space between panels
                 panel.spacing = unit(1, 'lines'),
                 # no background to wrap panels
                 strip.background = element_blank(),
                 strip.text = element_text(size = 14),
                 # panel labels outside x axis labels
                 strip.placement = 'outside',
                 # adjust x axis labels
                 axis.text.y = element_text(size = 14),
                 axis.text.x = element_text(size = 14, angle = 45, hjust = 1))

##### qc vis: photoplot #####

# area plots for cabr target spp in 1990 per plot (lots of figs ahead - for qc purpose)

# step 1 - wrangle
cabr1990 <- target1990 %>%
  filter(SiteCode %in% cabrsites) %>%
  # make pretty label columns
  mutate(Nice_num = paste('Plot ', Plot_num))

# step 2 - plot w/ lots of facets (one per target spp)
  # make loop eventually...start w single taxa focus
  # emphasize target taxon/a, de-emphasize others

for(i in c(1,3:length(zonelist))){
ggplot(data = filter(cabr1990, Zone == zonelist[i]),
       mapping = aes(x = SurveyYear, y = pct_cover, 
                # make target spp last (bottom bar on plots)
                 fill = fct_relevel(Scientific_name, zonenames[i], after = Inf))) +
  # bar stacked by spp group (Scientific_name)
  geom_bar(stat = 'identity', width = 1) + 
  # plots in grids by site and plot #
  facet_grid(SiteName ~ Nice_num) + 
  # axis and plot labels
  xlab('Year') + 
  ylab('Percent Cover') + 
  ggtitle(paste('Target Taxa:', zonenames[i])) +
  # colors
  scale_fill_manual(name = 'Taxon',
                    values = c(pnw_palette('Sailboat', 8, type = 'continuous'), 'gray20')) +
  theme_bw() + 
  lltheme

ggsave(paste('RI_Plots_Mar21/TARGET_Series_by_Plot_',targetlist[i], '.png', sep = ''))
}

# this vis doesn't work with 2000-aligned data. too many categories, proceed with 1990
# try forcats to limit # of categories that show up?

##### qc vis: transect #####

# wrangle data
transect <- transect %>%
  # remove NAs 
  filter(!is.na(Transect)) %>%
  # add transect type column
  mutate(Zone = if_else(Transect %in% 1:2, 'Red algal turf', 
                if_else(Transect %in% 3:4, 'Seagrass', 'Boa kelp'))) %>%
  mutate(Nice_zone = paste(Zone, Transect))

# get only cabr data
cabrtransect <- filter(transect, SiteCode %in% cabrsites)
  
# make a list of transect types over which to loop
transectnames <- unique(transect$Zone)

for(i in 1:length(transectnames)) {
  
ggplot(data = filter(cabrtransect, Zone == transectnames[1]), 
       mapping = aes(x = SurveyYear, y = pct_cover, 
                     fill = Scientific_name)) + 
    geom_bar(stat = 'identity', width = 1) +
    facet_grid(SiteName ~ Nice_zone) +
    # axis and plot labels
    xlab('Year') + 
    ylab('Percent Cover') + 
    ggtitle(paste('Target Taxa:', transectnames[i])) +
    # colors
    scale_fill_manual(name = 'Taxon',
                      values = c(pnw_palette('Sailboat', 
                              length(unique(cabrtransect$Scientific_name)), 
                              type = 'continuous'), 'gray20')) +
    theme_bw() + 
    lltheme
  ggsave(paste('RI_Plots_Mar21/TRANSECT_Series_by_Plot_',zonelist[i], '.png', sep = ''))
}

##### summary vis: photoplot #####

# no summarization
for(i in 1:length(targetlist)) {
  ggplot(data = filter(cabr1990, Zone == zonelist[i] & SpeciesCode %in% targetlist[i]),
         mapping = aes(x = SurveyYear, y = pct_cover)) + 
    geom_point(mapping = aes(color = Nice_num), size = 2) + 
    scale_color_manual(name = 'Plot Number',
                       values = c(cal_palette('kelp1', 7, type = 'continuous'))) +
    geom_smooth(color = 'black', method = 'lm', formula = y ~ x) +
    facet_wrap(~SiteName) + 
    # axis and plot labels
    xlab('Year') + 
    ylab('Percent Cover') + 
    ggtitle(paste('Target Taxa:', zonenames[i])) +
    # colors
    theme_bw() + 
    bigtexttheme
  
  # save reusulting plot in plots mar21 folder
  ggsave(paste('RI_Plots_Mar21/TARGET_TIMESERIES1_',targetlist[i], '.png', sep = ''))  
}

# get real creative - heatmap time series for all (slope = color for future idea?)

  # limit dataset to only target spp in target plot (ex - mussels in mussel plots)
  tgt_in <- cabr1990 %>% 
    # make zone list where spp codes == zone names
    mutate(Zone2 = if_else(Zone == 'CHT', 'CHTBAL',
                   if_else(Zone == 'MYT', 'MUSSEL',
                   if_else(Zone == 'SIL', 'SILCOM', 'POLPOL')))) %>%
    # get only where zone == target spp, and also tetrub in chtbal plots 
    filter(Zone2 == SpeciesCode | Zone2 == 'CHTBAL' & SpeciesCode == 'TETRUB') %>%
    # get mean % cover for each type across years
    group_by(SiteName, SurveyYear, Zone, Scientific_name) %>%
    summarise(
      cover_mean = mean(pct_cover),
      cover_sd = sd(pct_cover))
  
  # heatmap
  ggplot(data = tgt_in, 
         mapping = aes(x = SurveyYear, y = Scientific_name)) +
    geom_tile(mapping = aes(fill = cover_mean)) + 
    facet_wrap(~SiteName) +
    xlab('Year') +
    ylab('Target Species') +
    ggtitle('Average Target Taxa Cover over Time') +
    coord_cartesian(xlim = c(1990,2020))
  
    









transect_summary <- transect %>%
  filter(SiteCode %in% cabrsites) %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, SpeciesCode, Scientific_name) %>%
  
  
  

##### summary vis: target #####
# wrangle data
cabr1990_summary <- target1990 %>%
  # get CABR sites only in fall
  filter(SiteCode %in% cabrsites) %>%
  # get summary data by spp type and site/zone 
  group_by(SiteCode, SiteName, SurveyYear, Zone, SpeciesCode, Scientific_name) %>%
  summarise(
    cover_mean = mean(pct_cover),
    cover_sd = sd(pct_cover),
    cover_n = nrow(pct_cover))

# Plot line plot overviews w error

ggplot(data = cabr1990_summary,
       mapping = aes(x = SurveyYear, y = cover_mean, color = Scientific_name)) + 
  geom_line() + 
  facet_wrap(~ Zone)


##### qc vis: transect #####


