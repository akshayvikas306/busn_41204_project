library(tidyverse)

dev.new(width=5, height=4)

load('players.RData')

theme_update(plot.title = element_text(hjust = 0.5))

pd1 <- ggplot(pls, aes(x = overall, y = value)) +
  geom_point(col = 'gray', alpha = 0.5) + labs(x = 'Overall Player Rating',
                                               y = 'Player Value (EUR)',
                                               title = 'Player Values vs Rating') + 
  geom_smooth(method = 'lm') +
  theme_minimal()

pd2 <- ggplot(pls, aes(x = overall, y = ln_val)) +
  geom_point(col = 'gray', alpha = 0.5) + labs(x = 'Overall Player Rating',
                                               y = 'Player Log Value',
                                               title = 'Player Log Values vs Rating') + 
  geom_smooth(method = 'lm') +
  theme_minimal()

pd3 <-  ggplot(pls, aes(x=pos, y=ln_val)) + 
        geom_boxplot() + labs(x = 'Player Position',
                              y = 'Log Value',
                              title = 'Player Values distribution by position') + 
  theme_minimal()

fwds <- pls %>% filter(pos == 'FWD')
mids <- pls %>% filter(pos == 'MID')
defs <- pls %>% filter(pos == 'DEF')
gks <- pls %>% filter(pos == 'GK')

lm_fwd <- lm(fwds$ln_val ~ fwds$overall, data = fwds)
lm_mid <- lm(mids$ln_val ~ mids$overall, data = mids)
lm_def <- lm(defs$ln_val ~ defs$overall, data = defs)
lm_gks <- lm(gks$ln_val ~ gks$overall, data = gks)

pred_df_f <- data.frame(pred = predict(lm_fwd, fwds), over=fwds$overall)
pred_df_m <- data.frame(pred = predict(lm_mid, mids), over=mids$overall)
pred_df_d <- data.frame(pred = predict(lm_def, defs), over=defs$overall)
pred_df_g <- data.frame(pred = predict(lm_gks, gks), over=gks$overall)

colors <- c("Forwards" = "red", "Midfielders" = "blue", 
             "Defenders" = "green", "Goalkeepers" = "orange")

pd4 <-  ggplot(pls, aes(x = overall, y = ln_val, color = 'Legend')) +
  geom_point(color = 'gray', alpha = 0.1) + labs(x = 'Overall Player Rating',
                                                y = 'Player Log Value',
                                                title = 'Player Log Values vs Rating',
                                                colour = 'Regression Lines:') +
  geom_line(data = pred_df_f, aes(color ='Forwards', x=over, y=pred)) +
  geom_line(data = pred_df_m, aes(color='Midfielders', x=over, y=pred)) +
  geom_line(data = pred_df_d, aes(color='Defenders',x=over, y=pred)) +
  geom_line(data = pred_df_g, aes(color='Goalkeepers', x=over, y=pred)) +
  scale_color_manual(values = colors) + theme_minimal() +
  theme(legend.position='top',
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black"))


pd5 <- ggplot(pls, aes(x = potential, y = ln_val)) +
  geom_point(col = 'gray', alpha = 0.5) + labs(x = 'Player Potential',
                                               y = 'Player Log Value',
                                               title = 'Player Log Values vs Potential') + 
  geom_smooth(method = 'lm') +
  theme_minimal()

pls_year <-
  pls %>% group_by(year) %>% summarise(mlval = mean(ln_val)) %>%
  mutate(year = 2000 + year,
         mval = exp(mlval))

pd6 <- ggplot(pls_year, aes(x = year, y = mval)) +
  geom_point() + geom_line() + labs(x = 'Year',
                     y = 'Mean Player Value',
                    title = 'Player values over the years') +
  theme_minimal()

gridExtra::grid.arrange(pd1, pd2, pd3, pd4, pd5, pd6,
                        ncol = 2)

# top200 <- pls %>% arrange(desc(ln_val)) %>% head(200)
# 
# top200 %>% group_by(pos) %>% summarise(n()) #20GKs, 22 DEFs, 65 MIDs and 93FWDs
# 
# top200 %>% group_by(year) %>% summarise(n()) #Lowest from 2015 (8),
# #Highest from 19 (41)
# 
# #Highest Paid from France, Germany, Spain, Argentina & Brazil
# top200 %>% group_by(nationality) %>% summarise(num = n()) %>% arrange(desc(num))
# 
# #Highest Paid play in Spain & England
# top200 %>% group_by(league_name) %>% summarise(num = n()) %>% arrange(desc(num))
# 
# #Highest Paid likely to have contracts expiring soon
# top200 %>% group_by(contract_length) %>% summarise(num = n()) %>% arrange(desc(num))
