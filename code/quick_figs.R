library(ggplot2)
library(dplyr)
library(tidyr)
library(tidybayes)

'%ni%' = Negate('%in%')

doc_dat <- read.csv('/Users/travis/Documents/gits/protest_legitimacy/output/document_level_cors.csv')
plot_dat <- read.csv('/Users/travis/Documents/gits/protest_legitimacy/output/plotting_data_demogs.csv')
cor_dat <- read.csv('/Users/travis/Documents/gits/protest_legitimacy/output/source_level_cors.csv')

plot_dat %>%
  filter(trust_effect_z==0) %>%
  filter(document!='factor_analysis') %>%
  mutate(var = paste(measure, document, sep='\n')) %>%
  mutate(var = factor(var, levels=c('active_kill\ncaptions+titles',
                                    'passive_kill\ncaptions+titles',
                                    'Legitimizing\ncaptions+titles',
                                    'Legitimizing\ncaptions+titles+photos',
                                    'Legitimizing\nphotos',
                                    'innocence\ncaptions+titles',
                                    'mb\ncaptions+titles',
                                    'race\ncaptions+titles',
                                    'young\ncaptions+titles',
                                    'Police\ncaptions+titles',
                                    'Police\nphotos',
                                    'Protest\ncaptions+titles',
                                    'Protest\nphotos',
                                    'Negative Protest\ncaptions+titles'))) %>%
  filter(var %in% c('Police\ncaptions+titles',
                    'Police\nphotos',
                    'Protest\ncaptions+titles',
                    'Protest\nphotos',
                    'Negative Protest\ncaptions+titles')) %>%
  ggplot(aes(x=var, y=.value, group=Demog, shape=Demog, color=Demog)) + 
  geom_pointinterval(size=4, position=position_dodge(width=.25)) +
  xlab('') +
  ylab('Probability') +
  scale_x_discrete() +
  scale_color_viridis(discrete=T, end = .75) +
  theme(legend.title=element_blank(),
        legend.position='top',
        axis.text.x = element_text(angle=25, size=14, hjust=1)) -> pl

ggsave('/Users/travis/Documents/gits/protest_legitimacy/output/figures/fig1.jpeg',
       pl, width=11, height=6.5)

plot_dat %>%
  filter(trust_effect_z==0) %>%
  filter(document!='factor_analysis') %>%
  mutate(var = paste(measure, document, sep='\n')) %>%
  mutate(var = factor(var, levels=c('Legitimizing\ncaptions+titles',
                                    'Legitimizing\ncaptions+titles+photos',
                                    'mb\ncaptions+titles',
                                    'Legitimizing\nphotos',
                                    'active_kill\ncaptions+titles',
                                    'passive_kill\ncaptions+titles',
                                    'Negative Protest\ncaptions+titles',
                                    'innocence\ncaptions+titles',
                                    'race\ncaptions+titles',
                                    'young\ncaptions+titles',
                                    'Police\ncaptions+titles',
                                    'Police\nphotos',
                                    'Protest\ncaptions+titles',
                                    'Protest\nphotos'))) %>%
  mutate(var = fct_recode(var, 
                          `Active Kill\ncaptions+titles` = 'active_kill\ncaptions+titles',
                          `Passive Kill\ncaptions+titles` = 'passive_kill\ncaptions+titles',
                          `Innocence\ncaptions+titles` = 'innocence\ncaptions+titles',
                          `Michael Brown\ncaptions+titles` = 'mb\ncaptions+titles',
                          `Race\ncaptions+titles` = 'race\ncaptions+titles',
                          `Youth\ncaptions+titles` = 'young\ncaptions+titles',
                          `Michael Brown\nphotos` = 'Legitimizing\nphotos')) %>%
  filter(var %ni% c('Police\ncaptions+titles',
                    'Police\nphotos',
                    'Protest\ncaptions+titles',
                    'Protest\nphotos', 
                    'Legitimizing\ncaptions+titles',
                    'Negative Protest\ncaptions+titles')) %>%
  ggplot(aes(x=var, y=.value, group=Demog, shape=Demog, color=Demog)) + 
  geom_pointinterval(size=4, position=position_dodge(width=.25)) +
  xlab('') +
  ylab('Probability') +
  scale_x_discrete() +
  scale_color_viridis(discrete=T, end = .75) +
  theme(legend.title=element_blank(),
        legend.position='top',
        axis.text.x = element_text(angle=30, size=14, hjust=1)) -> pl

ggsave('/Users/travis/Documents/gits/protest_legitimacy/output/figures/fig2.jpeg',
       pl, width=11, height=6.5)

names(cor_dat)[2:13] <- c('Protest\nphotos', 'Police\nphotos', 
                          'Michael Brown\nphotos', 'Race\ncaptions+titles', 
                          'Innocence\ncaptions+titles', 
                          'Youth\ncaptions+titles',
                          'Michael Brown\ncaptions+titles', 
                          'Active Kill\ncaptions+titles', 
                          'Passive Kill\ncaptions+titles', 
                          'Police\ncaptions+titles',
                          'Protest\ncaptions+titles', 
                          'Negative Protest\ncaptions+titles')

M <- cor(cor_dat[,2:13], use='complete.obs')


pl <- corrplot(M, order='hclust', type='upper', method='color', 
               col=viridis(200), addCoef.col='black', number.cex=.6, 
               tl.col = "black", diag=F)

jpeg(filename = '/Users/travis/Documents/gits/protest_legitimacy/output/figures/fig3.jpeg')
corrplot(M, order='hclust', type='upper', method='color', 
         col=viridis(200), addCoef.col='black', number.cex=.6, 
         tl.col = "black", diag=F)
dev.off()

plot_dat %>%
  filter(trust_effect_z==0) %>%
  filter(document=='factor_analysis') %>%
  ggplot(aes(x=measure, y=.value, group=Demog, shape=Demog, color=Demog)) + 
  geom_pointinterval(size=4, position=position_dodge(width=.25)) +
  xlab('') +
  ylab('Factor values') +
  scale_x_discrete() +
  scale_color_viridis(discrete=T, end = .75) +
  theme(legend.title=element_blank(),
        legend.position='top',
        axis.text.x = element_text(angle=25, size=14, hjust=1)) -> pl

ggsave('/Users/travis/Documents/gits/protest_legitimacy/output/figures/fig4.jpeg',
       pl, width=11, height=6.5)
