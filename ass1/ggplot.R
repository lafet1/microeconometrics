# packages
library(ggplot2)


# a few plots
edulvl_ggpot <- ggplot(data = x, aes(edulvl, fill = 'indianred')) + geom_bar() + theme_minimal() + 
  labs(y = '# of people', x = 'Education level') +
  scale_x_discrete(limits = 1:6, labels = c("no degree","GED/high school", 
                                                 "college no degree", "associate degree",
                                                 'bachelor/professional', 'master/phd')) + 
  theme(axis.text.x = element_text(angle = 15)) + guides(fill = FALSE)

adover_ggplot <- ggplot(data = x %>% count(adover42), aes(adover42, weight = n, fill = 'indianred')) + 
  geom_bar() + theme_minimal() + 
  labs(y = '# of people', x = 'Can overcome illnes on his/her own') +
  scale_x_discrete(labels = c("disagree strongly", "disagree somewhat",
                                         "uncertain", "agree somewhat",
                                         'agree strongly', 'NA')) + 
  theme(axis.text.x = element_text(angle = 15)) + guides(fill = FALSE)

regions_ggplot <- ggplot(data = x %>% count(region12), aes(region12, weight = n, fill = 'indianred')) + 
  geom_bar() + theme_minimal() + 
  labs(y = '# of people', x = 'US region') +
  scale_x_discrete(labels = c("Northeast", "Midwest",
                              "South", "West")) + 
  theme(axis.text.x = element_text(angle = 15)) + guides(fill = FALSE)

married_ggplot <- ggplot(data = x %>% count(marry12x), aes(marry12x, weight = n, fill = 'indianred')) + 
  geom_bar() + theme_minimal() + 
  labs(y = '# of people', x = 'Marriage status') +
  scale_x_discrete(labels = c("married", "widowed",
                              "divorced", "separated", 'never married')) + 
  theme(axis.text.x = element_text(angle = 15)) + guides(fill = FALSE)

insurance_ggplot <- ggplot(data = x %>% count(inscov12), aes(inscov12, weight = n, fill = 'indianred')) + 
  geom_bar() + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 15)) + guides(fill = FALSE) +
  labs(y = '# of people', x = 'Type of insurance') +
  scale_x_discrete(labels = c("private", "public",
                              "uninsured"))
