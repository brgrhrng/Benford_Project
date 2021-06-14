# Libraries:----

library(dplyr)          #v 1.0.6
library(tidyverse)      #v 1.3.1
library(RColorBrewer)   #v 1.1-2
library(grid)           #v 4.02

getwd() #should be base project folder

#--- --- - -- - -- -



# Function Definitions:----

#Get the probability Benford's law would predict, for a leading digit d
Pd = function(d){
  return( log10( 1 + 1/(d) ) )
}

# Quick check of the above function
for (i in (1:9)){
  print(c(i,Pd(i)))
}

# Digit 1     Frequency
# 1.00000     0.30103
# 2.0000000   0.1760913
# 3.0000000   0.1249387
# 4.00000000  0.09691001
# 5.00000000  0.07918125
# 6.00000000  0.06694679
# 7.00000000  0.05799195
# 8.00000000  0.05115252
# 9.00000000  0.04575749

# Function to generate ggplot line layer, with rounded endcaps in the legend
# Taken directly from this stack overflow answer:
#   https://stackoverflow.com/questions/54817837/make-rounded-lineends-in-ggplot-both-in-plot-and-in-legend#54818836

GeomLine2 <- ggproto(
  "GeomLine2", GeomLine,
  draw_key =  function (data, params, size) {
    data$linetype[is.na(data$linetype)] <- 0
    segmentsGrob(0.3, 0.5, 0.7, 0.5, # I modified the x0 / x1 values here too, to shift
                 # the start / end points closer to the centre in order
                 # to leave more space for the rounded ends
                 gp = gpar(col = alpha(data$colour, data$alpha), 
                           lwd = data$size * .pt, 
                           lty = data$linetype, 
                           lineend = "round"),
                 arrow = params$arrow)
  })

geom_line2 <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, mapping = mapping, stat = stat, 
        geom = GeomLine2, # this is the only change from geom_line to geom_line2
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))}

#--- --- - -- - -- -




# Data loading/exploration (2000-2020):----

# Get the main data file (compiled by Dr. McCune)
df = read_csv("data/raw/county_pres_data.csv")

#Exploration
distinct(df,year) #2000-2020
distinct(df,state_po) %>% View() #includes district of columbia, plus apparently some NA values...
distinct(df,party) #no third party data, as of yet, but no NAs

#What's with all the state NAs?
df %>% rownames_to_column("i") %>% filter(is.na(state_po))
df %>% rownames_to_column("i") %>% filter(is.na(state_po)) %>% View()


#What rows are lacking counts?
# 24 such rows. Two are marked as from y2000/MO. remaining are also is.na(state)
df %>% filter(is.na(candidatevotes)) %>% View()


# Every year has at least some data, for every state & DC (excluding 2020, which was pre-filtered to only include interesting states)
df %>% filter(!is.na(state_po)) %>% group_by(year,state_po) %>% summarise(n()) %>% group_by(year) %>% summarise(n())

#--- --- - -- - -- -




# Data loading/exploration (1996):----

# 1) intialize data frame with names/prefixes for 32 interesting states
state_prefix = c("AL","AR","CA","CO","FL","GA","ID","IL","IN","IA","KS","KY","LA","MI","MN","MS","MO","MT","NE","NY",
                 "NC","ND","OH","OK","PA","SC","SD","TN","TX","VA","WV","WI")

state_name = c("Alabama", "Arkansas", "California", "Colorado", "Florida", "Georgia", "Idaho", "Illinois", "Indiana",
               "Iowa", "Kansas", "Kentucky", "Louisiana", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
               "Nebraska", "New_York", "North_Carolina", "North_Dakota", "Ohio", "Oklahoma", "Pennsylvania", "South_Carolina",
               "South_Dakota", "Tennessee", "Texas", "Virginia", "West_Virginia", "Wisconsin")

prefix_df = data.frame(state_prefix,state_name)

# 2) read in the loose files
loose_folder = "data/raw/states_1996"

df_combined = df
for (filename in list.files(loose_folder)){
  print("-----------")
  print(filename)
  state_df = read_csv(str_c(loose_folder,"/",filename))
  state_df = state_df %>%
    rename(republican=Republican,democrat=Democratic) %>%
    mutate(year=1996) %>% mutate(state=str_remove(filename,"_1996.csv")) %>% select(state,year,County,republican,democrat)  
  state_df = left_join(state_df,prefix_df,by=c("state"="state_name")) %>% rename(state_po=state_prefix)
  state_df = state_df %>%
    #pivot_longer(cols=republican:reform,names_to="party",values_to="candidatevotes") %>%
    pivot_longer(cols=republican:democrat,names_to="party",values_to="candidatevotes") %>%
    select(year,state_po,party,candidatevotes)
  df_combined = rbind(df_combined,state_df)
  print("-----------")
  
}

#Filter out any remaining Ross Perot (though there shouldn't be any)
df_combined = filter(df_combined,party!="reform")

#checking resulting df
df_combined %>% distinct(party) # democrat, republican
df_combined %>% filter(year==1996) %>% distinct(state_po) #32

df_combined %>% filter(year==1996) %>% View()

# The original county_pres_data.csv has  data for all states, for every included year, except 2020.
# The 2020 data has been filtered to only include the 32 interesting states

#So: we need to do some filtering
c_2020 = df_combined %>% filter(year==2020) %>% group_by(state_po) %>% summarise(counties=n()) %>% arrange(-counties)
states_keep = c_2020$state_po

df_interesting = df_combined %>% filter(state_po %in% states_keep) %>% rename(state=state_po) 

#verifying
arrange(distinct(df_interesting,state),state)$state == arrange(c_2020,state_po)$state_po


#Save a copy of the combined, filtered dataset to file
write.csv(arrange(df_interesting,year,state,party),"county_data_combined.csv")

#--- --- - -- - -- -





#Counting leading digits:----

# Pull out leading digits from candidatevotes
df_interesting = mutate( df_interesting, leading_digit = str_extract(as.character(candidatevotes), "[1-9]") )
df_interesting = mutate( df_interesting, leading_digit = as.integer(leading_digit) )

#Count occurences of each leading digit, for each year/state/party combo
#   Note: This will only generate rows for digits that are actually found
digit_counts = df_interesting %>% group_by(year,state,party,leading_digit) %>% summarise(n=n())

#Quick check that we haven't lost any counties (the below tables should be identical)
c_dg = digit_counts %>% group_by(year,state,party) %>% summarise(counties=sum(n))
c_og = df_interesting %>% group_by(year,state,party) %>% summarise(counties=n())
View(c_dg == c_og) #Should be a big table of TRUEs


#Adding missing count entries, for digits with 0 occurrences

# This can be done by (1) pivoting wide, so each digit gets its own column:
#   year state party        `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`   `9`  `NA`
#   2000 AL    democrat      15     9    10    18     4     4     2     2     3   NA
#   2000 AL    republican    21     9     5    12     4     9     2     3     2   NA
#   2000 AR    democrat      23    16    14     7     3     9     2     1    NA   NA
#   2000 AR    republican    20    20    17     3     8     1     1     4     1   NA
# And then (2), pivoting long again.

# 1)
#Pivot wide, to get a column for each digit
wide_counts = digit_counts %>% mutate(n=as.integer(n)) %>% mutate(leading_digit = str_c("digit_",as.character(leading_digit))) %>% pivot_wider(values_from=c(n),names_from=c(leading_digit))

#Replace NAs with 0
wide_counts = wide_counts %>%
  replace_na(list(
    digit_1=0,
    digit_2=0,
    digit_3=0,
    digit_4=0,
    digit_5=0,
    digit_6=0,
    digit_7=0,
    digit_8=0,
    digit_9=0,
    "NA"=0
    )) %>% rename(digit_NA="NA")

#Check that each state has the correct number of counties, as of 2020
wide_c_totals = wide_counts %>% mutate(total = sum(digit_1,digit_2,digit_3,digit_4,digit_5,digit_6,digit_7,digit_8,digit_9,digit_NA))
c_2020_totals = rbind(c_2020 %>% mutate(counties=counties/2),c_2020 %>% mutate(counties=counties/2)) %>% arrange(-counties)
c_2020_totals$counties == arrange(filter(wide_c_totals,year==2020),-total)$total #Should be all TRUE

#Checking if the # of counties is consistent w/in each year, for each state.
#If yes, that state should only have two entries in the following table:
tibble(wide_c_totals) %>% select(-c("year")) %>% distinct(state,party,total) %>% arrange(state) %>% View()
#   The # of counties in the dataset for CO, MO, and VA change slightly

# 2)
#Get back to long format. This should give rows of year/state/party/digit/count
#   But, we should now have rows cWorresponding to 0 counts
long_counts = wide_counts %>% pivot_longer(cols=digit_1:digit_NA) 

# Check # of digts represented for each year/state/party combo.
# Should always be 10, since we haven't filtered the NAs.
long_counts %>% group_by(year,state,party) %>% summarise(digits=n()) %>% View()

#Adding in total # of counties for each year/state/party combo
counties = long_counts %>% group_by(year,state,party) %>% summarise(total_counties=sum(value))
long_counts = left_join(long_counts,counties,by=c('year','state','party'))

#Adding in expected values under benford's law
long_counts = long_counts %>% mutate(name=str_remove(name,"digit_")) %>%
  mutate(digit=as.integer(name)) %>% mutate(expected = Pd(digit)*total_counties,expected_p=Pd(digit))

#Verifying
long_counts %>% View()

#--- --- - -- - -- -




#Chi-Square test, alpha 0.05 :----
# This will assess, for each year/state/party combo, whether the first digits adhere to Benford Predictions

chisq_alpha = 0.05

#Per-digit calcs
chisq_subcounts = long_counts %>% filter(name!="NA",!is.na(name)) %>%
  mutate(chisq=((value-expected)**2)/expected)

#Getting total chisq for each distribution
chisq_table = chisq_subcounts %>%
  group_by(year,state,party) %>%
  summarise(chisq=sum(chisq),n=n(),df=n()-1)

#Convert chisq results to p-values, and compare with alpha
#   if p < alpha, we can reject the null hypothesis (the distributions differ)
chisq_table = chisq_table %>%
  mutate(chisq_p = pchisq(q=chisq,df=df,lower.tail=FALSE) ) %>%
  mutate("differs_from_benford"=chisq_p < chisq_alpha)

#Checking visually
chisq_table %>% View()

#Adding chisq values into long_counts
long_counts = long_counts %>% left_join(select(chisq_table,year,state,party,differs_from_benford),
                          by=c("year","state","party")) %>%
  mutate(follows_benford = !differs_from_benford)

#Verifying
long_counts %>% View()

# #Save a copy to file
# write.csv(long_counts,"long_counts.csv")

#--- --- - -- - -- -






#Graphing distributions against Benford predictions:----

#Getting a dataframe of just the predicted distributions under benford, for each state/year
benford_dist = long_counts %>% group_by(year,state,digit,expected) %>% distinct(year,state,digit,expected)

#Graphing parameters
point_size = 3
sig_marker_size=3.5
line_alpha = 0.2
v_offset = .2
dem_color = "#1A85FF"
rep_color = "#D41159"

dem_pale = "#7dbef7"
rep_pale = "#b08495"


#LOOP TO SAVE STATE-BY-STATE DISTRIBUTION PLOTS
#   each individual plot will cover 8 states, with 1 state per row
for (i in c(1,9,17,25)){
  states_to_graph=levels(factor(long_counts$state))[i:(i+7)]
  
  print(as.character(i))
  print(states_to_graph)
  
  #get data for appropriate states
  long_counts_filtered = long_counts %>%
    filter(!is.na(digit)) %>%
    filter(state %in% states_to_graph) %>%
    group_by(state) %>%
    mutate(ymax=max(value))
  
  #get per-year/digit benford predictions each state
  benford_dist_filtered = benford_dist %>% filter(!is.na(digit)) %>% filter(state %in% states_to_graph)
  
  #Create faceted plot
  plot = ggplot(data=long_counts_filtered) +
    geom_line(data=long_counts_filtered,alpha=0.3,size=1.5,aes(x=digit,y=value,group=party,color=party)) +
    geom_point(data = benford_dist_filtered, aes(x=digit,y=expected), shape=4, color="#444444") + #Benford expected values
    facet_grid(cols=vars(year),rows=vars(state),scales = "free") +
    geom_point(data=filter(long_counts_filtered,party=="democrat",differs_from_benford==FALSE),
              aes(x=7.5,y=ymax*4/5),
              shape=22, size=sig_marker_size, alpha=1,
              color = dem_pale, fill = dem_pale) +
    geom_point(data=filter(long_counts_filtered,party=="republican",differs_from_benford==FALSE),
               aes(x=8.7,y=ymax*4/5),
               shape=22, size=sig_marker_size, alpha=1,
               color = rep_pale, fill = rep_pale) +
    scale_color_manual(values=c(dem_color,rep_color)) +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9)) +
    xlab("Leading Digit") +
    ylab("Count") +
    theme_classic() +
    theme(
      legend.position = "none"
    )

  #Save to project root
  ggsave(str_c("STATES_PLOT_",states_to_graph[1],"-",states_to_graph[8],".png"),
         width = 8.5,
         height = 5.5,
         units = "in")
  #At 8.5 x 5.5 in, two plots will fit on a standard page

  }

#--- --- - -- - -- -





#Chisq summary graphic (alpha 0.05):----

#Building chisq summary table
summary_table = long_counts %>%
  group_by(year,state,party) %>%
  summarise(follows_benford = sum(follows_benford),counties=mean(total_counties)) %>%
  mutate(follows_benford = case_when(
    follows_benford == 0 ~ FALSE,
    follows_benford == 10 ~ TRUE
  )) %>%
  mutate(facet_label=str_c(state," (n=",counties,")"))


#Writing the summary table to file
write.csv(summary_table,"chisq_results.csv")


#Goal-Make a "timeline" for each state/party, highlighting which years they followed benford, and leaving deviating years blank.
#   4 x 8 faceted grid; 1 facet = 1 state
#   each party gets a colored timeline

#Note- I intentionally converted non-benford rows to follow_benford=NA below, to create gaps in the timelines.
#   This will cause a few warning messages in the console.

chisq_summary = ggplot() +
  geom_line(data=filter(summary_table,party=="republican"),aes(x=year,y=.75,group=party),color=rep_color,size=.8,alpha=0.5) +
  geom_line(data=filter(summary_table,party=="democrat"),aes(x=year,y=.25,group=party),color=dem_color,size=.8,alpha=0.5) +
  geom_point(data=filter(mutate(summary_table,follows_benford=case_when(follows_benford==FALSE~NA,TRUE~follows_benford)),party=="republican"),
             aes(x=year,y=.75*follows_benford,group=party),color=rep_color,size=2.3) +
  geom_point(data=filter(mutate(summary_table,follows_benford=case_when(follows_benford==FALSE~NA,TRUE~follows_benford)),party=="democrat"),
             aes(x=year,y=.25*follows_benford,group=party),color=dem_color,size=2.3) +
  geom_line(data=filter(mutate(summary_table,follows_benford=case_when(follows_benford==FALSE~NA,TRUE~follows_benford)),party=="republican"),
            aes(x=year,y=.75*follows_benford,group=party),color=rep_color,size=2.7,lineend="round") +
  geom_line(data=filter(mutate(summary_table,follows_benford=case_when(follows_benford==FALSE~NA,TRUE~follows_benford)),party=="democrat"),
            aes(x=year,y=.25*follows_benford,group=party),color=dem_color,size=2.7,lineend="round") +
  scale_x_continuous(limits=c(1995.8,2020.2),breaks=c(1996,2000,2004,2008,2012,2016,2020)) +
  scale_y_continuous(limits=c(0,1),breaks=NULL) +
  facet_wrap(facets=c('state'),ncol=4) +
  xlab("") + ylab("") +
  ggtitle(str_c("Adherance to Benford Predictions (\u03C7\u00B2, \u03B1=",as.character(chisq_alpha),")")) + #other alpha: \U1D786\
  theme_classic() + theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5),
        panel.spacing.y = unit(-.1,"cm"),
        panel.grid.major = element_line(color = "#DDDDDD", size = 1, linetype=1),
        strip.background = element_blank(),
        panel.grid.minor=element_blank())#axis.text.x = element_text(angle = -90,vjust=.1,hjust=0))


#Notice how when you have a single Benford-following year (ex Alabama dems 2004), you get a tiny dot
#Whereas isolated non-benford years create these relatively wide gaps in the line (ex. Texas 2004)
chisq_summary

#This is kinda bad data viz, since it inflates your initial perception of benford deviations.

#So: I'll create a copy of summary_table, with extra values to extend the plotted lines right and left of their natural end points
line_margin = .7
summary_extra = summary_table %>% mutate(year_prev=year-line_margin) %>% mutate(year_after=year+line_margin) %>% pivot_longer(cols=c(year,year_prev,year_after))
summary_extra = summary_extra %>% filter(!(follows_benford==FALSE & name %in% c("year_prev","year_after")))
summary_extra = summary_extra %>% mutate(year=value)
summary_extra = summary_extra %>% mutate(v_offset=as.integer(party=="republican")/2)


#Plotting "padded" data
chisq_summary_padded = ggplot() +
  geom_line2(data=summary_extra,aes(x=year,y=.25+v_offset,group=party,color=party),size=.8,alpha=0.5) + #Thin background lines
  geom_point(data=mutate(summary_extra,
                         follows_benford=case_when(follows_benford==FALSE~NA,
                                                   TRUE~follows_benford)),
             aes(x=year,y=(.25+v_offset)*follows_benford,group=party,color=party),size=2.3) +
  geom_line2(data=mutate(summary_extra,follows_benford=case_when(follows_benford==FALSE~NA,TRUE~follows_benford)),
             aes(x=year,y=(.25+v_offset)*follows_benford,group=party,color=party),size=2.7,lineend="round") +
  scale_x_continuous(limits=c(1995.8,2020.2),breaks=c(1996,2000,2004,2008,2012,2016,2020)) +
  scale_y_continuous(limits=c(0,1),breaks=NULL) +
  facet_wrap(facets=c('state'),ncol=8) +
  xlab("") + ylab("") +
  ggtitle(str_c("Adherance to Benford Predictions (\u03C7\u00B2, \u03B1=",as.character(chisq_alpha),")")) + #other alpha: \U1D786\
  scale_color_manual(values=c(dem_color,rep_color),labels=c("Democrat","Republican")) + theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5),
        panel.grid.major = element_line(color = "#DDDDDD", size = 1, linetype=1),
        strip.background = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position="top",
        legend.margin = margin(0, 0, 0, 0),
        legend.title=element_blank(),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        plot.margin = unit(c(5,5,5,5),"mm"),
        axis.text.x = element_text(vjust=.1,size=8,angle=-60,hjust=.1)) #angle = -90,hjust=0

#Much better
chisq_summary_padded

#Saving formatted summary graph to file
ggsave(filename = "chisq_summary_8x4.png",
       plot=chisq_summary_padded,
       width=11,height=4.25,units="in")

#Additionally, save a "tall" (4x8 states) version
chisq_summary_padded_tall = chisq_summary_padded +
  facet_wrap(facets=c('state'),ncol=4) +
  theme(axis.text.x = element_text(vjust=.1,size=8,angle=-60,hjust=.1))

ggsave(filename = "chisq_summary_4x8.png",
       plot=chisq_summary_padded_tall,
       width=6.5,height=7,units="in")

#--- --- - -- - -- -






# Do we have a multiple comparison problem?:----

# Remember: alpha 0.05 means we're comfortable with a 5% chance of a type II error
# That probability compounds for every independent chi-square test you do... and we've performed *448*
#   This means we need to consider the possibility that some of the observed deviations are, due to random chance, false negatives

# One proposed solution to the multiple comparison problem is the "bonferroni correction."
# You divide your target alpha by the number of comparisons. If I understand it correctly, this
#   ensures that the probability of seeing *any* T2 errors approximately equals 5% (or whatever overall alpha you chose)
# This makes some intuitive sense, but it's also an incredibly conservative standard.
# For our dataset, bonferroni would suggest an alpha of ~0.0001.

# I'm going to consider alphas 0.1, 0.05, 0.001, and the bonferroni value for alpha 0.05

# We can pretty easily calculate an expected value for type II errors, by taking alpha * n

# In a perfect world, we would want to see 0 type 2 errors. This is what bonferroni basically aims for, with (1-alpha) certainty.
#   Every chi square test has an independent chance of 1-alpha to not give a false positive.
#   So, we can get this probability by taking (1-alpha) ^ n

#Calculating the above values, for the listed alphas
mc_table = data.frame(alpha=c(0.05,0.01,0.001,0.05/448)) %>%
  mutate(p_no_t2=round((1-alpha)**448,5),
         expected_t2=alpha*448) %>%
  mutate(alpha = as.character(as.double(alpha))
  )

# We can then compare the expected type II errors, with the actual deviations in the dataset at a given alpha
mc_expected_vs_obs = left_join(mc_table,
                               tibble(cbind(
                                 chisq_table %>% filter(chisq_p < 0.001) %>% ungroup() %>% summarise("0.001"=n()),
                                 chisq_table %>% filter(chisq_p < 0.01) %>% ungroup() %>% summarise("0.01"=n()),
                                 chisq_table %>% filter(chisq_p < 0.05) %>% ungroup() %>% summarise("0.05"=n()),
                                 chisq_table %>% filter(chisq_p < 0.05/448) %>% ungroup() %>% summarise("0.000111607142857143"=n())
                               )) %>% pivot_longer(cols="0.001":"0.000111607142857143",
                                                   names_to = "alpha",
                                                   values_to="observed deviations"))

#Notice that the observed deviations far exceed the type II errors we would predict based on chance, at any of these alphas.
#SO- multiple comparisons cannot explain away the deviations we see, and we don't have any real reason to lower alpha.


#Writing the summary table to file
write.csv(mc_expected_vs_obs,"multiple_comparisons.csv")

#--- --- - -- - -- -






# p-value histograms:----

#Testing significance at varying alphas
#   I've left the bonferroni alpha off this chart, since 0.05 already seems stringent enough.

multi_chisq = chisq_table %>% filter(is.na(chisq_p)==FALSE) %>% mutate(
  sig_at = case_when(
    chisq_p < 0.001 ~ "0.001",
    chisq_p < 0.01 ~ "0.01",
    chisq_p < 0.05 ~ "0.05",
    TRUE ~ "Never significant"
  )
)

#Remember these significance groups are nested; significance at 0.05 means significance at all lower alphas
alphas_n = cbind(
  chisq_table %>% filter(chisq_p < 0.001) %>% ungroup() %>% summarise("alpha0.001"=n()),
  chisq_table %>% filter(chisq_p < 0.01) %>% ungroup() %>% summarise("alpha0.01"=n()),
  chisq_table %>% filter(chisq_p < 0.05) %>% ungroup() %>% summarise("alpha0.05"=n()),
  chisq_table %>% filter(chisq_p >= 0.05) %>% ungroup() %>% summarise("alphaNever_Significant"=n())
) %>% pivot_longer(cols=alpha0.001:alphaNever_Significant,
                   names_to = "alpha",
                   values_to="n") %>%
  mutate(alpha=str_remove(alpha,"alpha"),
         percent=n/448,
         line=row_number()) %>%
  mutate(alpha=case_when(
    alpha == "Never_Significant" ~ "insig",
    TRUE ~ alpha
  )) %>% mutate(
    percent=str_c(round(percent*100,2),"%")
  )


#Verifying
alphas_n

#Generating histogram of p-values
binw = 0.005
histogram_p = ggplot(data=multi_chisq) + 
  geom_histogram(data=filter(multi_chisq,!differs_from_benford),aes(x=chisq_p,group=sig_at,fill=sig_at),binwidth = binw,fill="#dacfe8") +
  geom_histogram(data=filter(multi_chisq,differs_from_benford),aes(x=chisq_p,group=sig_at,fill=sig_at,color="#FFFFFF"),binwidth = binw) +
  xlab(str_c("\u03C7\u00B2 p (binwidth=",binw,")")) + ylab("Count") +
  ggtitle("Distribution of p-values") +
  scale_x_continuous(limits=c(0-binw/2,1+binw/2),expand = expansion(mult = c(.01, .01))) +
  scale_y_continuous(limits=c(0,15), expand = expansion(mult = c(.01, .01))) +
  guides(color = FALSE) +
  scale_fill_brewer(name = "Significant at \u03B1", palette = "YlOrRd",direction = - 1) +
  geom_rect(fill="#7f4659",xmin=0.75,xmax=0.91,ymax=14.7-.2,ymin=14.8-2.5) +
  geom_text(data=tail(rbind(alphas_n,c("\u03B1","count","percent",0)),1),hjust=0,
            aes(x=0.75,y=14.8,label=str_c(
              str_pad(alpha,12,side="right"),
              str_pad(n,12,side="right"),
              str_pad(percent,10,"right")
            ))) +
  geom_text(data=filter(alphas_n,n != 384),aes(x=0.75,y=14.7-line/2,hjust=0,
                                               label=str_c(
                                                 str_pad(alpha,12,side="right"),
                                                 str_pad(n,12,side="right"),
                                                 str_pad(percent,10,"right")
                                               ))) +
  geom_text(data=filter(alphas_n,n != 384),aes(x=0.75,y=14.7-line/2,hjust=0,
                                               label=str_c(
                                                 str_pad(alpha,12,side="right"),
                                                 str_pad(n,12,side="right"),
                                                 str_pad(percent,10,"right")
                                               ),
                                               color=alpha)) +
  geom_text(data=filter(alphas_n,n == 384),aes(x=0.75,y=14.7-line/2,hjust=0,
                                               label=str_c(
                                                 str_pad(alpha,12,side="right"),
                                                 str_pad(n,12,side="right"),
                                                 str_pad(percent,10,"right")
                                               )),
            color="#dacfe8") +
  scale_color_brewer(name = "Differs at \u03B1=", palette = "YlOrRd",direction = - 1) +
  theme_classic() +
  theme(
    panel.spacing = unit(0, "cm"),
    plot.title = element_text(hjust=0.5),
    legend.position="top"
  )

#Saving to file
ggsave("p_histogram.png",
       plot=histogram_p,
       width=12,height=5.663,
       units="in")

#--- --- - -- - -- -

#Histogram, faceted by party:----

alphas_n_party = cbind(
  chisq_table %>% filter(chisq_p < 0.001) %>% ungroup() %>% group_by(party) %>% summarise("alpha0.001"=n()),
  chisq_table %>% filter(chisq_p < 0.01) %>% ungroup() %>% group_by(party) %>% summarise("alpha0.01"=n()),
  chisq_table %>% filter(chisq_p < 0.05) %>% ungroup() %>% group_by(party) %>% summarise("alpha0.05"=n()),
  chisq_table %>% filter(chisq_p >= 0.05) %>% ungroup() %>% group_by(party) %>% summarise("alphaNever_Significant"=n())
) %>% pivot_longer(cols=alpha0.001:alphaNever_Significant,
                   names_to = "alpha",
                   values_to="n") %>%
  mutate(alpha=str_remove(alpha,"alpha"),
         percent=n/224,
         line=case_when(row_number() < 5 ~ row_number(),
                        TRUE ~ as.integer(row_number()-4))) %>%
  mutate(alpha=case_when(
    alpha == "Never_Significant" ~ "insig",
    TRUE ~ alpha
  )) %>% mutate(
    percent=str_c(round(percent*100,2),"%")
  )


binw = 0.005
histogram_p_party = ggplot(data=multi_chisq) +
  #geom_density(aes(x=chisq_p)) +
  geom_histogram(data=filter(multi_chisq,!differs_from_benford),aes(x=chisq_p,group=sig_at,fill=sig_at),binwidth = binw,fill="#dacfe8") +
  geom_histogram(data=filter(multi_chisq,differs_from_benford),aes(x=chisq_p,group=sig_at,fill=sig_at,color="#FFFFFF"),binwidth = binw) +
  xlab(str_c("\u03C7\u00B2 p (binwidth=",binw,")")) + ylab("Count") +
  ggtitle("Distribution of p-values") +
  scale_x_continuous(limits=c(0-binw/2,1+binw/2),expand = expansion(mult = c(.01, .01))) +
  scale_y_continuous(limits=c(0,10.2), expand = expansion(mult = c(.01, .01))) +
  scale_fill_brewer(name = "\u03B1", palette = "YlOrRd",direction = - 1) +
  geom_rect(fill="#7f4659",xmin=0.75,xmax=0.91,ymax=8.7-.2,ymin=8.7-2.5) +
  geom_text(data=tail(rbind(alphas_n_party,
                            c("democrat","\u03B1","count","percent",0),
                            c("republican","\u03B1","count","percent",0)),2),
            hjust=0,
            aes(x=0.75,y=8.9,label=str_c(
              str_pad(alpha,12,side="right"),
              str_pad(n,12,side="right"),
              str_pad(percent,10,"right")
            ))) +
  geom_text(data=filter(alphas_n_party,alpha != "insig"),aes(x=0.75,y=8.7-line/2,hjust=0,
                                                             label=str_c(
                                                               str_pad(alpha,12,side="right"),
                                                               str_pad(n,12,side="right"),
                                                               str_pad(percent,10,"right")
                                                             ))) +
  geom_text(data=filter(alphas_n_party,alpha != "insig"),aes(x=0.75,y=8.7-line/2,hjust=0,
                                                             label=str_c(
                                                               str_pad(alpha,12,side="right"),
                                                               str_pad(n,12,side="right"),
                                                               str_pad(percent,10,"right")
                                                             ),
                                                             color=alpha)) +
  geom_text(data=filter(alphas_n_party,alpha == "insig"),aes(x=0.75,y=8.7-line/2,hjust=0,
                                                             label=str_c(
                                                               str_pad(alpha,12,side="right"),
                                                               str_pad(n,12,side="right"),
                                                               str_pad(percent,10,"right")
                                                             )),
            color="#dacfe8") +
  geom_label(data=distinct(ungroup(multi_chisq),party),
             mapping = aes(label=str_to_title(party),x=0.5,y=9.5,size=10,color="#000000")) +
  scale_color_brewer(name = "\u03B1", palette = "YlOrRd",direction = - 1) +
  guides(color = FALSE, size = FALSE) +
  #geom_hline(yintercept=10.2) +
  facet_wrap(facets="party",ncol=1) +
  theme_bw() +
  theme(
    #plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.spacing = unit(.3, "cm"),
    plot.title = element_text(hjust=0.5),
    legend.position="top",
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


#Saving to file
ggsave("p_histogram_party.png",
       plot=histogram_p_party,
       width=12.1,height=8,
       units="in")





# Plotting chi-square values by year:----

# For exploratory purposes, it's worth plotting the overall chi square values over time (instead of just the binary result--ie benford vs differing) 
# If they consistently were right on the border, for example, that would be worth noting.

yearly_chisq = ggplot(data=chisq_table) +
  geom_line(data=data.frame(x=c(1996,2020),y=c(15.507,15.507),chisq_crit=factor("chisq_crit")),aes(x=x,y=y,linetype=chisq_crit),alpha=1) +
  geom_line(data=data.frame(x=c(1996,2020),y=c(20.090,20.090),chisq_crit=factor("chisq_crit_0.01")),aes(x=x,y=y,linetype=chisq_crit),alpha=0.5) +
  geom_line(data=data.frame(x=c(1996,2020),y=c(26.124,26.124),chisq_crit=factor("chisq_crit_0.001")),aes(x=x,y=y,linetype=chisq_crit),alpha=0.25) +
  geom_line2(aes(group=party,color=party,x=year,y=chisq),size=1.2,lineend="round",alpha=.5) +
  scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2016,2020)) +
  scale_color_manual(values=c(dem_color,rep_color),labels=c("Democrat","Republican")) +
  scale_linetype_manual(name="",labels=c("Critical Threshhold (\u03B1 0.05)","\u03B1 0.01","\u03B1 0.001"),values=c(5,5,5)) +
  labs(title="\u03C7\u00B2 values by year") + #,subtitle="Lines correspond to critical values for \u03B1=0.05,\u03B1=0.1 (8 df)"
  xlab("Year") + ylab("\u03C7\u00B2 (actual values vs Benford predictions)") +
  guides(colour = guide_legend(order=2,override.aes = list(alpha = 1))) +
  facet_wrap(facets="state",nrow=4) +
  theme_classic() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_text(vjust=.1,size=6), #angle = -90,hjust=0
        legend.position="top",
        legend.title=element_blank(),
        axis.text.x.bottom = element_text(vjust=.1,size=8,angle=-60,hjust=.1))

#Saving to file
ggsave("chisq_yearly.png",
     plot=yearly_chisq,
     width=8,height=8,
     units="in")






# Plotting chi-square residuals:----

# Looking at per-digit residuals from the chi-square test, you can "eyeball" which specific digits
#   are furthest from the benford distribution. This may be helpful as a reference.

# From a statistical standpoint, it would be more useful to assess per-digit benfordness with a Z test (see below)

chisq_crit = 15.507 #for alpha 0.05, with 8 df

chisq_residuals = chisq_subcounts %>% left_join(rename(chisq_table,chisq_total=chisq))

for (i in c(1,9,17,25)){
  print(i)
  states_to_graph=levels(factor(long_counts$state))[i:(i+7)]
  print(states_to_graph)
  
  res = ggplot(data=(chisq_residuals %>% filter(state %in% states_to_graph))) +
    #geom_rect(ymin=-1,xmin=-1,xmax=10,alpha=0.01,aes(group=party,ymax=chisq_total,color=party,fill=party)) +
    geom_line2(aes(group=party,color=party,x=digit,y=chisq),size=2,lineend="round",alpha=.5) +
    geom_line(data=data.frame(x=c(1,9),y=c(chisq_crit,chisq_crit),chisq_crit=factor("chisq_crit")),aes(x=x,y=y,linetype=chisq_crit),alpha=1) +
    geom_point(data=filter(chisq_residuals,party=="democrat",differs_from_benford==TRUE,state %in% states_to_graph),
                aes(x=7.5,y=chisq_crit+3),
                shape="X", size=3, alpha=1,
                color = dem_pale, fill = dem_pale,show.legend=FALSE) +
    geom_point(data=filter(chisq_residuals,party=="republican",differs_from_benford==TRUE,state %in% states_to_graph),
                aes(x=8.5,y=chisq_crit+3),
                shape="X", size=3, alpha=1,
                color = rep_pale, fill = rep_pale,show.legend=FALSE) +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9)) +
    scale_y_continuous(limits=c(0,20)) +
    ggtitle(str_c("\u03C7\u00B2 residuals (per digit)")) +
    xlab("Leading Digit") + ylab("(observed - expected)\u00B2") +
    scale_color_manual(values=c(dem_color,rep_color),labels=c("Democrat","Republican")) +
    scale_fill_manual(values=c(dem_color,rep_color),labels=c("Democrat","Republican")) +
    scale_linetype_manual(name="",labels="Critical Threshhold (\u03B1 0.05)",values=c(5)) +
    guides(colour = guide_legend(order=2,override.aes = list(alpha = 1))) +
    facet_grid(rows=vars(state),cols=vars(year)) +
    theme_bw() +
    theme(
        panel.spacing = unit(.3, "cm"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_blank(),
        strip.background = element_rect(fill = "white", colour = "black", size = rel(2)),      
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust=0.5),
        legend.title=element_blank(),
        legend.position="top",
      )
  
  ggsave(str_c("Residuals_",states_to_graph[1],"-",states_to_graph[8],".png"),
         plot=res,
         width=8,height=8,
         units="in")
}


#--- --- - -- - -- -





# Z-test, alpha 0.05: ----

# The z-test evaluates each digit's adherance to Benford (or some other arbitrary distribution), independently
#     Nigrini (1996) applies this to accounting data. I followed their equation 7 to get z-scores
# if z-score > z_critical, the observed proportion differs significantly from the benford prediction

#NOTE- it's entirely possible for the z-test to identify a couple differing digits, even when the overall distribution follows benford per chi-square

z_critical = 1.96 #alpha 0.05
z_table = long_counts %>% filter(!is.na(digit)) %>% 
  mutate(obs_proportion=value/total_counties) %>%
  mutate(sd_digit=sqrt(expected_p * (1-expected_p) / total_counties)) %>%
  mutate(abs_residual=abs(obs_proportion-expected_p)) %>%
  mutate(cont_correction=1/(2*total_counties)) %>%
  mutate(apply_correction=(cont_correction<abs_residual)) %>%
  mutate(z_stat=(abs_residual-cont_correction*apply_correction)/sd_digit) %>%
  mutate(z_sig=(z_stat>z_critical))

z_table = z_table %>% select(year,state,party,digit,total_counties,expected,observed=value,expected_p,obs_proportion,sd_digit,abs_residual,cont_correction,apply_correction,z_stat,z_sig,follow_chisquare=follows_benford)

#Visual check of significant rows
z_table %>% filter(z_sig==TRUE) %>% View()


#Generate z-test summary graphics (8 states/plot, as with distribution plots)
for (i in c(1,9,17,25)){
  states_to_graph=levels(factor(long_counts$state))[i:(i+7)]
  
  print(as.character(i))
  print(states_to_graph)
  
  #get data for appropriate states
  long_counts_filtered = long_counts %>%
    filter(!is.na(digit)) %>%
    filter(state %in% states_to_graph) %>%
    group_by(state) %>%
    mutate(ymax=max(value))
  
  #get per-year/digit benford predictions each state
  benford_dist_filtered = benford_dist %>% filter(!is.na(digit)) %>% filter(state %in% states_to_graph)
  
  #Create faceted plot
  #   x in the top right indicates *chi-square* (whole distribution) deviation from benford
  ggplot(data=filter(z_table,state %in% states_to_graph)) + 
    geom_rect(ymin=z_critical,ymax=5,xmin=-1,xmax=10,fill="white",color=NA,alpha=0.02) +
    geom_point(data=filter(z_table,state %in% states_to_graph,z_sig),aes(group=party,color=party,x=digit,y=z_stat),color="white",size=3,alpha=1,show.legend = FALSE) +
    geom_line2(aes(x=digit,y=z_stat,group=party,color=party),size=1.2,alpha=0.8) +
    geom_rect(ymin=-1,ymax=z_critical,xmin=-1,xmax=10,fill="light grey",color=NA,alpha=0.07) +
    geom_point(data=filter(z_table,state %in% states_to_graph,z_sig),aes(group=party,color=party,x=digit,y=z_stat),shape=1,size=3,alpha=1,show.legend = FALSE) +
    geom_point(data=filter(z_table,party=="democrat",follow_chisquare==FALSE,state %in% states_to_graph),
               aes(x=7.5,y=4.5),
               shape="X", size=4, alpha=1,
               color = dem_pale, fill = dem_pale) +
    geom_point(data=filter(z_table,party=="republican",follow_chisquare==FALSE,state %in% states_to_graph),
               aes(x=8.7,y=4.5),
               shape="X", size=4, alpha=1,
               color = rep_pale, fill = rep_pale) +
    geom_line(data=data.frame(x=c(1,9),y=c(z_critical,z_critical),zcrit=factor("z critical")),aes(x=x,y=y,linetype=zcrit),alpha=1) +
    facet_grid(cols=vars(year),rows=vars(state),scales = "free") +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9)) +
    scale_y_continuous(limits = c(0,5)) +
    scale_color_manual(name="",values=c(dem_color,rep_color),labels=c("Democrat","Republican"),aesthetics="colour") +
    scale_linetype_manual(name="",labels="Z critical",values=c(5)) +
    guides(colour = guide_legend(order=2,override.aes = list(alpha = 1))) +
    xlab("Digit") + ylab("Z Statistic") +
    labs(title="Adherance to Benford Predictions, per digit (Z test, \u03B1=0.05)") +
    theme_bw() + #theme_classic() +
    theme(
      panel.spacing = unit(.3, "cm"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line = element_blank(),
      panel.border = element_blank(),
      strip.background = element_rect(fill = "white", colour = "black", size = rel(2)),      
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(hjust=0.5),
      legend.position="top",
    )
  
  #Save to project root
  ggsave(str_c("Z_plot_",states_to_graph[1],"-",states_to_graph[8],".png"),
         width = 8.5,
         height = 8,
         units = "in")
  
}


# The z results might be more readable in a table, honestly.
# So, it's worth saving a copy of the z table to a .csv
write.csv(z_table,"Z_scores.csv")

#Saving a copy with only the deviating rows
z_table %>% filter(z_sig==TRUE) %>% write.csv("Z_scores_significant.csv")

#--- --- - -- - -- -





# Misc averages/summary statistics:----
# Lastly, some summary statistics we considered. These don't generally capture the dataset as well as the above graphs,
# but they're worth at least mentioning here.

# Overall, averaging across all years, what do the first digit distributions look like for each party?
digit_averages = long_counts %>%
  filter(!is.na(digit)) %>%
  mutate(obs_proportion=value/total_counties) %>%
  group_by(digit,party) %>%
  summarise("Expected"=mean(expected_p),"Observed"=mean(obs_proportion))

# I mean, both look at least Benford-like.... but this doesn't give a great picture of the state-by-state picture
ggplot(data=digit_averages) +
  geom_col(data=filter(digit_averages,party=="democrat"),
           aes(group=party,x=digit,y=Expected),fill="#BB9999",alpha=0.5) +
  geom_point(aes(x=digit,y=Observed,color=party)) +
  ggtitle("\"Average\" distributions across all states, by party") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9)) +
  ylab("Frequency") + xlab("Leading Digit") +
  scale_color_manual(values=c(dem_color,rep_color),labels=c("Democrat","Republican")) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        panel.spacing.y = unit(-.1,"cm"),
        strip.background = element_blank(),
        panel.grid.minor=element_blank())#axis.text.x = element_text(angle = -90,vjust=.1,hjust=0))


#state averages, using chi-square
state_avgs=long_counts %>% filter(digit==1) %>% #differs_from_benford is per yearly distribution, so any digit will do
  group_by(year,state,party,differs_from_benford) %>% arrange(state,differs_from_benford) %>% 
  select(year,state,party,differs_from_benford) %>%
  group_by(state,party,differs_from_benford) %>% summarise(years=n()) %>%
  pivot_wider(names_from=differs_from_benford,values_from=years) %>%
  rename("follow_benford"='FALSE','deviate'='TRUE') %>%
  mutate("percent_follow"=follow_benford/7)

#Average of the averages
#   OR, grand mean of (% of years following benford), pooling by state
state_avgs %>% group_by(party) %>% summarise(mean(percent_follow))


#Lastly: how many individual digits follow/don't follow benford (Z-test data)?
digit_z_avgs=z_table %>%
  group_by(year,state,party,z_sig) %>%
  summarise(percent=n()) %>%
  pivot_wider(names_from=z_sig,values_from=c("percent")) %>%
  rename("follow_benford"='FALSE','deviate'='TRUE') %>%
  mutate("percent_follow"=follow_benford/9)

#NAs->0 (not strictly necessary)
digit_z_avgs = digit_z_avgs %>% mutate(percent_follow=case_when(
  is.na(percent_follow)~0,
  TRUE ~ percent_follow
))

#Getting another "grand mean"
digit_z_avgs %>% group_by(party) %>% summarise(mean(percent_follow))

#The vast majority of digits, technically speaking, do conform to Benford. Of course, this is kind of a misleading measure...
#   It tells you only about the number of digits (1-9) that deviate, not about the magnitude of deviation.
#   In other words, these percentages really don't say much about the overall applicability of Benford.