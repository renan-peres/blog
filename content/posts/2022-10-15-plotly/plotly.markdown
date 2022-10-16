---
title: plotly
author: ''
date: '2022-10-15'
slug: index.en-us
categories: []
---

<script src="/rmarkdown-libs/htmlwidgets/htmlwidgets.js"></script>
<script src="/rmarkdown-libs/pymjs/pym.v1.js"></script>
<script src="/rmarkdown-libs/widgetframe-binding/widgetframe.js"></script>
<script src="/rmarkdown-libs/htmlwidgets/htmlwidgets.js"></script>
<script src="/rmarkdown-libs/pymjs/pym.v1.js"></script>
<script src="/rmarkdown-libs/widgetframe-binding/widgetframe.js"></script>

This data was retrieved from the [World Bank](https://datacatalog.worldbank.org/search/dataset/0038026/Worldwide-Governance-Indicators).

<!--more-->

## Raw Data

[View Resume](/files/resume.pdf)

``` r
print(df)
## # A tibble: 7,704 × 28
##    Country N…¹ Count…² Indic…³ Indic…⁴ `1996` `1998` `2000` `2002` `2003` `2004`
##    <chr>       <chr>   <chr>   <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
##  1 Afghanistan AFG     Contro… CC.EST  -1.29  -1.18  -1.27  -1.25  -1.34  -1.35 
##  2 Afghanistan AFG     Contro… CC.NO.…  2      2      2      2      3      5    
##  3 Afghanistan AFG     Contro… CC.PER…  4.30   8.02   4.79   4.76   4.76   6.40 
##  4 Afghanistan AFG     Contro… CC.PER…  0      0      0      0      0      0    
##  5 Afghanistan AFG     Contro… CC.PER… 27.4   33.7   30.9   32.8   19.0   15.3  
##  6 Afghanistan AFG     Contro… CC.STD…  0.341  0.324  0.347  0.353  0.270  0.213
##  7 Afghanistan AFG     Govern… GE.EST  -2.18  -2.10  -2.17  -1.59  -1.13  -0.910
##  8 Afghanistan AFG     Govern… GE.NO.…  1      1      1      2      2      4    
##  9 Afghanistan AFG     Govern… GE.PER…  0      0      0      2.16  11.4   18.9  
## 10 Afghanistan AFG     Govern… GE.PER…  0      0      0      0      1.62   5.97 
## # … with 7,694 more rows, 18 more variables: `2005` <dbl>, `2006` <dbl>,
## #   `2007` <dbl>, `2008` <dbl>, `2009` <dbl>, `2010` <dbl>, `2011` <dbl>,
## #   `2012` <dbl>, `2013` <dbl>, `2014` <dbl>, `2015` <dbl>, `2016` <dbl>,
## #   `2017` <dbl>, `2018` <dbl>, `2019` <dbl>, `2020` <dbl>, `2021` <dbl>,
## #   ...28 <lgl>, and abbreviated variable names ¹​`Country Name`,
## #   ²​`Country Code`, ³​`Indicator Name`, ⁴​`Indicator Code`
```

## Prepararation

During the preparation I had to pivot the years into a single variable and add the corresponding continent to each country.

``` r

# Pivoting
df_wgi <- df %>%
          pivot_longer(-c(1:4), names_to = "year", values_to = "value") %>% 
          clean_names() %>% 
          select(-indicator_code)

# Adding Continents
df_wgi <- df_wgi %>% 
            inner_join(countrycode::codelist %>% select(continent, cow.name), 
                       by = c(country_name = "cow.name"))
print(df_wgi)
## # A tibble: 145,152 × 6
##    country_name country_code indicator_name                  year  value conti…¹
##    <chr>        <chr>        <chr>                           <chr> <dbl> <chr>  
##  1 Afghanistan  AFG          Control of Corruption: Estimate 1996  -1.29 Asia   
##  2 Afghanistan  AFG          Control of Corruption: Estimate 1998  -1.18 Asia   
##  3 Afghanistan  AFG          Control of Corruption: Estimate 2000  -1.27 Asia   
##  4 Afghanistan  AFG          Control of Corruption: Estimate 2002  -1.25 Asia   
##  5 Afghanistan  AFG          Control of Corruption: Estimate 2003  -1.34 Asia   
##  6 Afghanistan  AFG          Control of Corruption: Estimate 2004  -1.35 Asia   
##  7 Afghanistan  AFG          Control of Corruption: Estimate 2005  -1.45 Asia   
##  8 Afghanistan  AFG          Control of Corruption: Estimate 2006  -1.45 Asia   
##  9 Afghanistan  AFG          Control of Corruption: Estimate 2007  -1.61 Asia   
## 10 Afghanistan  AFG          Control of Corruption: Estimate 2008  -1.67 Asia   
## # … with 145,142 more rows, and abbreviated variable name ¹​continent
```

## Indicators

Given the indicators available in the dataset I selected to analyzie the following:

``` r
df_wgi %>% 
distinct(indicator_name) %>% 
filter(indicator_name %in% c("Government Effectiveness: Estimate", 
                             "Control of Corruption: Estimate",
                             "Regulatory Quality: Estimate", 
                             "Rule of Law: Estimate"))
## # A tibble: 4 × 1
##   indicator_name                    
##   <chr>                             
## 1 Control of Corruption: Estimate   
## 2 Government Effectiveness: Estimate
## 3 Regulatory Quality: Estimate      
## 4 Rule of Law: Estimate
```

## Plots

To better analyze the dataset, I decided to group the countries available in the dataset by continent and
compare them accordingly.

You can also embed plots. See Figure <a href="#fig:pie">1</a> for example:

``` r
par(mar = c(0, 1, 0, 1))
pie(
  c(280, 60, 20),
  c('Sky', 'Sunny side of pyramid', 'Shady side of pyramid'),
  col = c('#0292D8', '#F7EA39', '#C4B632'),
  init.angle = -50, border = NA
)
```

<div class="figure">

<img src="/posts/2022-10-15-plotly/plotly_files/figure-html/pie-1.png" alt="A fancy pie chart." width="960cm" />
<p class="caption">
Figure 1: A fancy pie chart.
</p>

</div>

``` r
# Effectiveness
  df_wgi %>% 
  filter(indicator_name == "Government Effectiveness: Estimate") %>% 
  mutate(indicator_name = str_replace(indicator_name, 
                                      pattern =  "Government Effectiveness: Estimate",
                                      replacement = "Government Effectiveness")) %>% 
  group_by(continent) %>% 
  filter(continent != "NA",
         year > 2015) %>% 
  ggplot(aes(x = year, y = value, fill = continent)) +
  geom_boxplot(na.rm = T) +
  # scale_fill_viridis_d(option = "mako") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", size = .8) +
  labs(title = "Effectiveness", x = element_blank(), y = element_blank()) +
  # theme_bw() +
  cowplot::theme_cowplot() +
  theme(axis.text = element_text(size = 14, face = "bold"),
        title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_blank()) -> p1


# Control of Corruption
  df_wgi %>% 
    filter(indicator_name == "Control of Corruption: Estimate") %>% 
    mutate(indicator_name = str_replace(indicator_name, 
                                        pattern =  "Control of Corruption: Estimate",
                                        replacement = "Control of Corruption")) %>% 
    group_by(continent) %>% 
    filter(continent != "NA",
           year > 2015) %>% 
    ggplot(aes(x = year, y = value, fill = continent)) +
    geom_boxplot(na.rm = T) +
    # scale_fill_viridis_d(option = "mako") +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed", size = .8) +
    labs(title = "Control of Corruption", x = element_blank(), y = element_blank()) +
    # theme_bw() +
    cowplot::theme_cowplot() +
    theme(axis.text = element_text(size = 14, face = "bold"), 
          title = element_text(size = 20),
          legend.text = element_text(size = 16),
          legend.title = element_blank()) -> p2


# Regulatory Quality
  df_wgi %>% 
    filter(indicator_name == "Regulatory Quality: Estimate") %>% 
    mutate(indicator_name = str_replace(indicator_name, 
                                        pattern =  "Regulatory Quality: Estimate",
                                        replacement = "Regulatory Quality")) %>% 
    group_by(continent) %>% 
    filter(continent != "NA",
           year > 2015) %>% 
    ggplot(aes(x = year, y = value, fill = continent)) +
    geom_boxplot(na.rm = T) +
    # scale_fill_viridis_d(option = "mako") +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed", size = .8) +
    labs(title = "Regulatory Quality", x = element_blank(), y = element_blank()) +
    # theme_bw() +
    cowplot::theme_cowplot() +
    theme(axis.text = element_text(size = 14, face = "bold"), 
          title = element_text(size = 20),
          legend.text = element_text(size = 16),
          legend.title = element_blank()) -> p3

  
# Rule of Law
  df_wgi %>% 
    filter(indicator_name == "Rule of Law: Estimate") %>% 
    mutate(indicator_name = str_replace(indicator_name, 
                                        pattern =  "Rule of Law: Estimate",
                                        replacement = "Rule of Law")) %>% 
    group_by(continent) %>% 
    filter(continent != "NA",
           year > 2015) %>% 
    ggplot(aes(x = year, y = value, fill = continent)) +
    geom_boxplot(na.rm = T) +
    # scale_fill_viridis_d(option = "mako") +
    scale_fill_brewer(palette = "Dark2") +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed", size = .8) +
    labs(title = "Rule of Law", x = element_blank(), y = element_blank()) +
    # theme_bw() +
    cowplot::theme_cowplot() +
    theme(axis.text = element_text(size = 14, face = "bold"), 
          title = element_text(size = 20),
          legend.text = element_text(size = 16),
          legend.title = element_blank()) -> p4
```

[View Plotly](/files/plotly.html)

``` r
plotly::ggplotly(p1) -> p1
## Warning: `gather_()` was deprecated in tidyr 1.2.0.
## ℹ Please use `gather()` instead.
## ℹ The deprecated feature was likely used in the plotly package.
##   Please report the issue at <https://github.com/plotly/plotly.R/issues>.
widgetframe::frameWidget(p1)
```

<div id="htmlwidget-1" style="width:100%;height:960cmpx;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"url":"/posts/2022-10-15-plotly/plotly_files/figure-html//widgets/widget_unnamed-chunk-2.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

``` r
library(leaflet)
leaflet::leaflet() %>% 
  addTiles() -> l
  widgetframe::frameWidget(l)
```

<div id="htmlwidget-2" style="width:100%;height:960cmpx;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"url":"/posts/2022-10-15-plotly/plotly_files/figure-html//widgets/widget_unnamed-chunk-3.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>
