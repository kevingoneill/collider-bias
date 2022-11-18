library(tidyverse)
library(brms)
library(tidybayes)

## load and format the data
d <- read_csv('experiment1-data.csv') %>%
    slice(-c(1:2)) %>%
    select(ResponseId, starts_with('restaurant'), starts_with('gpa'),
           `Duration (in seconds)`, age, gender, attention_check) %>%
    rename(id=ResponseId,
           duration=`Duration (in seconds)`) %>%
    pivot_longer(restaurant_binary:gpa_ternary,
                 names_to=c('vignette', 'variable'), names_sep='_') %>%
    filter(!is.na(value)) %>%
    mutate(value=factor(value, levels=c('less', 'same', 'more'), ordered=TRUE),
           variable=factor(variable))

## make a response histogram
d.summary <- d %>%
    group_by(vignette, variable, value, .drop=FALSE) %>%
    count() %>%
    group_by(vignette, variable) %>%
    mutate(proportion=n/sum(n))

ggplot(d.summary, aes(x=variable, y=proportion, fill=value)) +
    geom_col(position='stack') +
    facet_wrap(~ vignette) +
    theme_classic()


## test for differences
m <- brm(bf(value ~ cs(variable*vignette)), d, family=sratio,
         prior=set_prior('normal(0, 1)', class='b'), cores=4)
summary(m, prior=TRUE)

## assuming no category-specific effects
##m <- brm(bf(value ~ variable*vignette), d, family=cumulative,
##         prior=set_prior('normal(0, 10)', class='b'), cores=4)

draws.prob <- d %>%
    distinct(vignette, variable) %>%
    add_epred_draws(m) %>%
    mutate(.category=factor(.category, levels=c('less', 'same', 'more'), ordered=TRUE))

## histogram
ggplot(draws.prob, aes(x=variable)) +
    geom_col(aes(y=proportion, fill=value), position='dodge', data=d.summary) +
    stat_pointinterval(aes(y=.epred, group=.category), position='dodge') +
    geom_hline(yintercept=0.3, linetype='dashed') +
    facet_wrap(~ vignette) +
    theme_classic()
ggsave('experiment1.png', width=6, height=4)

## stacked histogram
draws.prob %>%
    group_by(.draw) %>%
    pivot_wider(names_from=.category, values_from=.epred) %>%
    mutate(more=less+same+more,
           same=less+same) %>%
    pivot_longer(less:more, names_to='.category', values_to='.epred') %>%
    filter(.category != 'more') %>%    
    ggplot(aes(x=variable)) +
    geom_col(aes(y=proportion, fill=value), position=position_stack(reverse=TRUE),
             data=d.summary %>% mutate(value=factor(value, levels=c('less', 'same', 'more')))) +
    stat_pointinterval(aes(y=.epred, group=.category), position='dodge') +
    facet_wrap(~ vignette) +
    theme_classic()
ggsave('experiment1-stacked.png', width=6, height=4)



draws.pred <- d %>%
    distinct(vignette, variable) %>%
    add_linpred_draws(m) %>%
    mutate(.category=factor(.category, levels=1:2,
                            labels=c('less vs. same/more', 'same vs. more')))

## plot latent means (fixed to 0 for binary/gpa)
draws.pred %>%
    ggplot(aes(y=.linpred, x=.category, fill=variable,
               side=ifelse(variable=='binary', 'left', 'right'))) +
    stat_halfeye(point_interval=median_hdi, position=position_dodge(-.5)) +
    geom_hline(yintercept=0, linetype='dashed') +
    facet_wrap(~ vignette) +
    theme_classic()
ggsave('experiment1-latent.png', width=6, height=4)

## plot contrasts of latent means
draws.pred %>%
    compare_levels(.linpred, by=variable) %>%
    ggplot(aes(y=.linpred, x=.category)) +
    stat_halfeye(point_interval=median_hdi) +
    geom_hline(yintercept=0, linetype='dashed') +
    facet_wrap(~ vignette) +
    theme_classic()
