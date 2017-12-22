
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(scales) # to access breaks/formatting functions


## Read in csv data
game_df <- read.csv(file = "PackersBears.csv",header = TRUE,sep = ",")



## Convert Date variable, create new Result flag and Cumulative Series Score
game_df <- game_df %>% mutate(Date = as.Date(x =Date, "%b%d%Y"),
                              ResultFlag = ifelse(Result == "W",
                                                  yes = 1,
                                                  no = ifelse(Result == "T",
                                                              yes = 0,
                                                              no = -1)
                                                  ),
                              LegendLabel = ifelse(Result == "W",
                                                  yes = "GB Win Margin",
                                                  no = ifelse(Result == "T",
                                                              yes = "Tie",
                                                              no = "CHI Win Margin")
                              ),
                              ScoreDifference = TmScore - OppScore
                              ) %>% 
  arrange(Date) %>%
  mutate(SeriesScore = cumsum(ResultFlag))


series_high <- max(abs(game_df$SeriesScore))

score_high <- max(abs(game_df$ScoreDifference))

scale_value <- series_high/ score_high

midpoint_days <- (max(game_df$Date) - min(game_df$Date))/2

midpoint <- min(game_df$Date) + midpoint_days




ggplot(data = game_df, aes(x=Date, y=ScoreDifference)) + 
  geom_bar(stat = "identity", width = 200, aes(fill=LegendLabel))+ 
  geom_line(data = game_df, 
            aes(y=SeriesScore/scale_value, lty = 'Series Lead'), 
            colour = "white", 
            size = 2) +
  

  scale_fill_manual("legend", values = c("GB Win Margin" = "dark green", 
                                         "CHI Win Margin" = "orangered1", 
                                         "Tie" = "dark gray")) +

  scale_y_continuous(limits = c(-1*score_high -6, score_high+6), 
                     sec.axis = sec_axis(~.*scale_value, name = "Series Lead")) +
  geom_hline(yintercept=0,colour = "white") +
  
  xlab("") +
  ylab("Win Margin") + 
  annotate("text", 
           x = midpoint, 
           y = 30, 
           label = "Packers",
           fontface = 2,
           size = 25, 
           alpha = 0.3,
           colour = "dark green") + 
  
  annotate(geom = "text", 
           x = midpoint, 
           y = -30, 
           label = "Bears",
           fontface = 2,
           size = 25,
           alpha = 0.3,
           colour = "orangered2") + 
  theme_tufte(base_size = 12, 
              base_family='GillSans') +
  scale_x_date(breaks = date_breaks("5 years"), 
               labels = date_format("%Y")) +
  
  theme(plot.background = element_rect(fill = "gray25"),
        legend.position="bottom",
        text = element_text(colour = "white"),
        legend.background = element_blank(), 
        legend.title = element_blank(), 
        axis.text.y = element_text(colour="white"),
        axis.text.x = element_text(angle=-45, colour="white"))  + 
        
  ggtitle(label = "Packers vs Bears All-Time Series", subtitle = "The Packers and Bears rivalry is the oldest existing NFL rivalry. Green Bay leads the series 96-94-6.The graph below shows 
the results for each Packers-Bears game. The direction of the bars represent who won each matchup, while the length of the 
bars represent each game's win differential.The white line displays the series lead difference.")
