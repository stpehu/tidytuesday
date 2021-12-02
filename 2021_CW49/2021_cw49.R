library(tidyverse)
library(lubridate)
library(tidytuesdayR)
library(cowplot)

#tuesdata <- tidytuesdayR::tt_load('2021-11-30')
#saveRDS(tuesdata,file="2021-11-30")
tuesdata <-readRDS(file="2021-11-30")

matches <- tuesdata$matches
#gives a table 1237 x 24 

#filter tied matches and matches without date
# results in 1217   24
matchesTidy <- matches %>% mutate(match_date=mdy(match_date)) %>%
        filter(!str_detect(winner,"tied")) %>%
        filter(!is.na(match_date))

#----------------------------------------------------------
#We will check whether it is an advantage to play at home
# and which team is most strong when playing home

matchesTidy <-matches%>%filter(team1_away_or_home=="home" | team2_home_away=="home") %>%
  select (team1,team2,team1_away_or_home, team2_home_away,winner,ground_country) %>%
  mutate(home_team=case_when(team1_away_or_home=="home"~team1,team2_home_away=="home"~team2))%>%
  mutate(away_team=case_when(team1_away_or_home=="away"~team1,team2_home_away=="away"~team2)) %>%
  select(-c("team1","team2","team1_away_or_home","team2_home_away")) %>%
  mutate(winnerplace=ifelse(home_team==winner,"homewin","awaywin"))
#View(matchesTidy)

whenPlayHome <- matchesTidy%>% group_by(home_team,winnerplace) %>%
  summarise(win = n())%>%
  ungroup()%>%
  group_by(home_team) %>%
  mutate(allGames = sum(win)) %>%
  ungroup() %>%
  mutate(win=ifelse(winnerplace == "awaywin",-1*win,win), allGames=ifelse(winnerplace == "awaywin",-1*allGames,allGames))

whenPlayAway <- matchesTidy%>% group_by(away_team,winnerplace) %>%
  summarise(win = n())%>%
  ungroup()%>%
  group_by(away_team) %>%
  mutate(allGames = sum(win)) %>%
  ungroup() %>%
  mutate(win=ifelse(winnerplace == "homewin",-1*win,win), allGames=ifelse(winnerplace == "homewin",-1*allGames,allGames))

#View(whenPlayHome)
#View(whenPlayAway)


plothome <-whenPlayHome %>%
  ggplot(aes(x=home_team,y=win)) + 
  geom_bar(aes(fill=winnerplace),stat="identity")+coord_flip() +
  geom_errorbar(aes(x=home_team,ymin=allGames,ymax=allGames))+
  labs(title="Results when playing in homecounty",
       x ="Country ", y="Games won (pos = home / negative = away)",
       subtitle = "Most teams win more games when playing at home"
  ) +
  scale_fill_discrete(name="Won / Lost",
                      breaks=c("homewin","awaywin" ),
                      labels=c("won", "lost")) +
  theme(
    plot.title =  element_text(size = rel(1.1), hjust = .5,  face = "bold", margin = margin(t = -2, b = 1)),
    plot.subtitle =  element_text(size = rel(.8), hjust = 0, face = "bold.italic", margin = margin(b = 2)),
    text = element_text(color = "darkgray"),
    
    strip.text = element_text(size=12,color = "darkgreen"),
    
    axis.text.y =  element_text(color = "green",size = rel(1.0), hjust = .1, ),
    axis.title.y = element_text(  color = "darkgreen",face = "bold", size = rel(1.8)),
    
    axis.text.x =  element_text(angle = 90, hjust = 1,color = "green",size = rel(0.9)),
    axis.title.x = element_text(  color = "darkgreen",face = "bold", size = rel(1.1)),
    
    plot.background = element_rect(fill = "#411441", color = NA),
    plot.margin = margin(10,10,30,10)
  )+
  geom_text(angle = 12,hjust=ifelse((whenPlayHome$win > 0),-0.8,1.8), vjust=0.2,size = rel(2.88),label=whenPlayHome$win)

plotaway <-whenPlayAway %>%
  ggplot(aes(x=away_team,y=win)) + 
  geom_bar(aes(fill=winnerplace),stat="identity")+coord_flip() +
  geom_errorbar(aes(x=away_team,ymin=allGames,ymax=allGames))+
  labs(title="Results when playing in opp county",
       x ="Country ", y="Games won (pos = away / negative = home)",
       subtitle = "When playing away the result usually is worse"
  ) +
  scale_fill_discrete(name="Won / Lost",
                      breaks=c("homewin","awaywin" ),
                      labels=c("lost", "won")) +
  theme(
    plot.title =  element_text(size = rel(1.1), hjust = .5,  face = "bold", margin = margin(t = -2, b = 1)),
    plot.subtitle =  element_text(size = rel(.8), hjust = 0, face = "bold.italic", margin = margin(b = 2)),
    text = element_text(color = "darkgray"),
    
    strip.text = element_text(size=12,color = "darkgreen"),
    
    axis.text.y =  element_text(color = "green",size = rel(1.0), hjust = .1, ),
    axis.title.y = element_text(  color = "darkgreen",face = "bold", size = rel(1.8)),
    
    axis.text.x =  element_text(angle = 90, hjust = 1,color = "green",size = rel(.9)),
    axis.title.x = element_text(  color = "darkgreen",face = "bold", size = rel(1.1)),
    
    plot.background = element_rect(fill = "#411441", color = NA),
    plot.margin = margin(10,10,30,10)
  )+
  geom_text(angle = 12,hjust=ifelse((whenPlayAway$win > 0),-0.8,1.8), vjust=0.2,size = rel(2.88),label=whenPlayAway$win)


title <- ggdraw() + draw_label("Check the homestrength of the teams", fontface='bold',colour="darkgreen",size = rel(23.88)) +
  theme(
    plot.background = element_rect(fill = "#411441", color = "#111111")
  )
a1 <-plot_grid(title, nrow = 1, labels = c(""),rel_heights = c(0.4))
a2 <-plot_grid(plothome, plotaway, ncol=2, labels = c("", ""),rel_heights = c(1,1))
a3_1 <-ggdraw() +draw_image("cricket.jpg",x = .1, y = -.1, scale = .9)+
  theme(
    plot.background = element_rect(fill = "#411441", color = "#411441")
  )
explainText<-str_c("This graphics show the results when one team is playing in hometown. Usually this will",
                   "result in better result. We also check what is the result when play in county of the opposite.",
                   "",
                   "Lets have a look at \"India\":",
                   "India playes 96 games in homecounty - They won 54 which is 55% !!",
                   "They played 93 games at oposites place - and won 39 (42%). ",
                   "",
                   "Also interesting; Netherlands and Canada never played a game at home",
                   sep="\n")

a3_2 <- ggdraw() +draw_text(explainText,  size = 9,colour="white",just="left",hjust = 0, vjust = .5,x=.1)+
  theme(
    plot.background = element_rect(fill = "#411441", color = "#411441")
  )
a3 <- plot_grid(a3_1, a3_2, ncol=2, labels = c("", ""),rel_widths = c(.3,.6))
a4 <- ggdraw() +draw_text("World Cup Cricket tidytuesday / shugg@shugg.de (data from https://www.espncricinfo.com/)", x = 0.5, y = 0.5, size = 14)+
  theme(
    plot.background = element_rect(fill = "#411441", color = "#411441")
  )
plot_grid(a1,a2,a3,a4, nrow=4, labels = c("", "","",""),rel_heights = c(.1,1,.4,.1))

ggsave("cricket.png", width=10, height=6)
