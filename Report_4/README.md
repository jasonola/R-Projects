FDS Final Project: Report \#4
================

#### Libraries

``` r
library(tidyverse)
library(readxl)
library(kableExtra)
library(infer)
```

#### Setting the main plot theme

``` r
theme_set(theme_minimal())
```

#### Loading dataset

``` r
athletes <- as_tibble(read_excel("olympics.xlsx", 
    sheet = "athletes")) %>% 
  rename(athlete_id = ID)
country <- as_tibble(read_excel("olympics.xlsx", 
    sheet = "country"))
games <- as_tibble(read_excel("olympics.xlsx", 
    sheet = "games"))
medals <- as_tibble(read_excel("olympics.xlsx", 
    sheet = "medals"))
```

## Part 1

##### Have some athletes competed for different countries over time?

``` r
num_mult_country <- athletes %>% 
  left_join(country, by = "athlete_id") %>% 
  select(athlete_id,Name,NOC) %>% 
  distinct(athlete_id,Name,NOC) %>% 
  filter(duplicated(athlete_id)) %>% 
  distinct(athlete_id, Name) %>% 
  nrow()
```

Yes there are 1570 athletes with different countries over time.

## Part 2

##### Who are the ten athletes that took part in most games?

``` r
athletes %>% 
  left_join(country, by = "athlete_id") %>% 
  count(Name) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  rename("Number of appearances" = n) %>% 
  kable() %>% 
  add_header_above(c("Top 10 athletes by number of appearances"=2)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F,
                fixed_thead = T)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; position: sticky; top:0; background-color: #FFFFFF;" colspan="2">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

Top 10 athletes by number of
appearances

</div>

</th>

</tr>

<tr>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

Name

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">

Number of appearances

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Ian Millar

</td>

<td style="text-align:right;">

10

</td>

</tr>

<tr>

<td style="text-align:left;">

Afanasijs Kuzmins

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;">

Hubert Raudaschl

</td>

<td style="text-align:right;">

9

</td>

</tr>

<tr>

<td style="text-align:left;">

Aleksandr Vladimirovich Popov

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

Chen Jing

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

Durward Randolph Knowles

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

Francisco Boza Dibos

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

Josefa Idem-Guerrini

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

Lesley Allison Thompson-Willie

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:left;">

Li Na

</td>

<td style="text-align:right;">

8

</td>

</tr>

</tbody>

</table>

Note that there are other athletes with 8 appearances, I just trunkated
at 10.

## Part 3

##### What athlete(s) kept a Gold medal for the longest time?

``` r
longest_gm_kept_ath <- athletes %>% 
  left_join(medals, by = "athlete_id") %>% 
  select(athlete_id,Name,Sport,Games,Medal) %>% 
  filter(Medal == "Gold") %>% 
  separate(Games,c("[0-9]+","[A-Za-z]")) %>% 
  rename(Year="[0-9]+", Season = "[A-Za-z]") %>% 
  mutate(Year = as.integer(Year)) %>%
  group_by(athlete_id,Name,Sport) %>% 
  distinct(Year) %>%
  filter(n()>1) %>%
  mutate(lagyear = Year-4) %>%
  mutate(consec = if_else(lagyear == lag(Year),1,0)) %>%
  mutate(consec = if_else(is.na(consec),0,consec)) %>% 
  mutate(strike_id = cumsum(consec)) %>% 
  mutate(strike_id = strike_id - cummax((consec == 0)*strike_id)+1) %>% 
  arrange(desc(strike_id)) %>% 
  head(1)
```

The athlete who kept a gold medal for the longest time is Birgit
Fischer-Schmidt for 5 consecutive editions in Canoeing.

## Part 4

##### What country(ies) kept a Gold medal for the longest time?

``` r
longest_gm_kept_country <- athletes %>% 
  left_join(medals, by = "athlete_id") %>% 
  select(athlete_id,Team,Sport,Games,Medal) %>% 
  filter(Medal == "Gold") %>% 
  separate(Games,c("[0-9]+","[A-Za-z]")) %>% 
  rename(Year="[0-9]+", Season = "[A-Za-z]") %>% 
  mutate(Year = as.integer(Year)) %>% 
  group_by(Team,Sport) %>% 
  distinct(Year) %>% 
  arrange(Year) %>% 
  filter(n()>1) %>% 
  mutate(lagyear = Year-4) %>%
  mutate(consec = if_else(lagyear == lag(Year),1,0)) %>% 
  mutate(consec = if_else(is.na(consec),0,consec)) %>% 
  mutate(strike_id = cumsum(consec)) %>% 
  mutate(strike_id = strike_id - cummax((consec == 0)*strike_id)+1) %>% 
  arrange(desc(strike_id)) %>% 
  head(1)
```

The country who kept a gold medal for the longest time is Romania for 10
consecutive editions in Gymnastics.

## Part 5

##### Who are the ten athletes that competed in the most events ?

``` r
athletes %>% 
  left_join(medals, by = "athlete_id") %>% 
  select(athlete_id,Name,Event) %>% 
  group_by(athlete_id,Name) %>% 
  distinct(Event) %>% 
  count(athlete_id) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ungroup() %>% 
  select(Name,n) %>% 
  rename("Number of different events" = n) %>% 
  kable() %>% 
  add_header_above(c("Top 10 athletes who competed in the most events"=2)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F,
                fixed_thead = T)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; position: sticky; top:0; background-color: #FFFFFF;" colspan="2">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

Top 10 athletes who competed in the most
events

</div>

</th>

</tr>

<tr>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

Name

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">

Number of different events

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Ioannis Theofilakis

</td>

<td style="text-align:right;">

33

</td>

</tr>

<tr>

<td style="text-align:left;">

Alexandros Theofilakis

</td>

<td style="text-align:right;">

28

</td>

</tr>

<tr>

<td style="text-align:left;">

Gustaf Eric Carlberg

</td>

<td style="text-align:right;">

24

</td>

</tr>

<tr>

<td style="text-align:left;">

Gustaf Vilhelm Carlberg

</td>

<td style="text-align:right;">

22

</td>

</tr>

<tr>

<td style="text-align:left;">

Frangiskos D. Mavrommatis

</td>

<td style="text-align:right;">

22

</td>

</tr>

<tr>

<td style="text-align:left;">

Paul Van Asbroeck

</td>

<td style="text-align:right;">

20

</td>

</tr>

<tr>

<td style="text-align:left;">

Lars Jrgen Madsen

</td>

<td style="text-align:right;">

19

</td>

</tr>

<tr>

<td style="text-align:left;">

Lon Ernest Moreaux

</td>

<td style="text-align:right;">

19

</td>

</tr>

<tr>

<td style="text-align:left;">

Marie Joseph “Raoul” le Borgne de Boigne

</td>

<td style="text-align:right;">

18

</td>

</tr>

<tr>

<td style="text-align:left;">

Lon douard Johnson

</td>

<td style="text-align:right;">

18

</td>

</tr>

</tbody>

</table>

Note that there are other athletes with 18 appearances, I just trunkated
at 10.

## Part 6

``` r
num_medals <- medals %>% 
  left_join(country, by = "athlete_id") %>% 
  separate(Games.x,c("[0-9]+","[A-Za-z]")) %>% 
  rename(Year="[0-9]+", Season = "[A-Za-z]") %>% 
  mutate(Year = as.integer(Year)) %>% 
  filter(Medal %in% c("Gold","Silver","Bronze")) %>% 
  mutate(Medal = "Some medal") %>% 
  count(Medal,NOC) %>% 
  arrange(desc(n)) %>% 
  head(15) %>% 
  select(NOC,n) %>% 
  rename("Number_of_medals"=n) %>% 
  mutate(NOC = as.factor(NOC)) 
num_medals %>%
  rename("Number of medals" = Number_of_medals) %>%
  kable() %>%
  add_header_above(c("Number of medals per country and per year"=2)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F,
                fixed_thead = T)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; position: sticky; top:0; background-color: #FFFFFF;" colspan="2">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

Number of medals per country and per
year

</div>

</th>

</tr>

<tr>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

NOC

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">

Number of
medals

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

USA

</td>

<td style="text-align:right;">

9859

</td>

</tr>

<tr>

<td style="text-align:left;">

GER

</td>

<td style="text-align:right;">

4709

</td>

</tr>

<tr>

<td style="text-align:left;">

URS

</td>

<td style="text-align:right;">

4579

</td>

</tr>

<tr>

<td style="text-align:left;">

ITA

</td>

<td style="text-align:right;">

4017

</td>

</tr>

<tr>

<td style="text-align:left;">

GBR

</td>

<td style="text-align:right;">

3748

</td>

</tr>

<tr>

<td style="text-align:left;">

FRA

</td>

<td style="text-align:right;">

3402

</td>

</tr>

<tr>

<td style="text-align:left;">

SWE

</td>

<td style="text-align:right;">

3220

</td>

</tr>

<tr>

<td style="text-align:left;">

AUS

</td>

<td style="text-align:right;">

2911

</td>

</tr>

<tr>

<td style="text-align:left;">

HUN

</td>

<td style="text-align:right;">

2702

</td>

</tr>

<tr>

<td style="text-align:left;">

RUS

</td>

<td style="text-align:right;">

2653

</td>

</tr>

<tr>

<td style="text-align:left;">

CAN

</td>

<td style="text-align:right;">

2505

</td>

</tr>

<tr>

<td style="text-align:left;">

NOR

</td>

<td style="text-align:right;">

2225

</td>

</tr>

<tr>

<td style="text-align:left;">

NED

</td>

<td style="text-align:right;">

2174

</td>

</tr>

<tr>

<td style="text-align:left;">

FIN

</td>

<td style="text-align:right;">

2069

</td>

</tr>

<tr>

<td style="text-align:left;">

CHN

</td>

<td style="text-align:right;">

1960

</td>

</tr>

</tbody>

</table>

## Part 7

##### Is there are relationship between country and the probability of winning a medal?

``` r
medals %>% 
  left_join(country, by = "athlete_id") %>%
  mutate(Medal = if_else(is.na(Medal),"No medal","Medal")) %>% 
  count(NOC,Medal) %>% 
  group_by(NOC) %>% 
  mutate(perc = n/sum(n)) %>% 
  filter(Medal == "Medal") %>% 
  arrange(desc(perc)) %>% 
  mutate(perc = round(perc,2)) %>% 
  head(15) %>% 
  ggplot(aes(x = reorder(NOC,desc(perc)),y = perc))+
  geom_bar(stat = "identity")+
  labs(title = "Relative percentage of medal won for each country in the top 15",
       subtitle = "For all time",
       x = "NOC",
       y = "Percentage",
       caption = "Note that USSR is dismandled since 1990")+
  scale_y_continuous(labels = scales::percent)+
  theme_light()
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> Looking at
the charts I would think there is a relationship between country and
whether they win a medal or not, especially if that country is the USA.
But by my knowledge, I would think that it changed a lot the last years,
for example China is strong now and has higher chances of winning a
medal today. I’ll make a Chi-test to make sure.

``` r
chi_t <- medals %>% 
  left_join(country,by = "athlete_id") %>% 
  mutate(Medal = if_else(is.na(Medal),"No medal","Medal")) %>% 
  distinct(NOC,Games.x,Medal,Sport) %>% 
  chisq_test(Medal~NOC) %>% 
  mutate(p_value = scales::percent(p_value))
```

We see in the test that p-value : 0% is below 5%, we can reject the
null-hypothesis saying “There is no relationship between country and
whether they win a medal or not”.  
So there are indeed countries with more chance of having a medal
overall, but this is probably due to the fact that countries like the
USA or Germany have a lot of athletes in many different sports.

## Part 8

``` r
body_means <- athletes %>% 
  left_join(medals, by = "athlete_id") %>% 
  select(Height,Weight,Sport) %>% 
  filter(!is.na(Height),
         !is.na(Weight)) %>% 
  mutate(Height=Height/100) %>%
  mutate(BMI = Weight/(Height*Height)) %>%
  group_by(Sport) %>%
  summarise(mean_h = mean(Height),
            mean_w = mean(Weight),
            mean_b = mean(BMI))
body_means %>% 
  ggplot(aes(mean_w,mean_h,label = Sport))+
  geom_point()+
  geom_label(data = body_means %>%  filter(
                 mean_h %in% range(mean_h) | 
                 mean_w %in% range(mean_w) |
                 mean_b %in% range(mean_b)
               ),
             size = 2,
             nudge_x = 1,
             nudge_y = -0.012)+
  labs(title = "Average height and weight of competitors per sport",
       subtitle = "Labels are the sports names for extreme height, weight and BMI values",
       x = "Average weight",
       y = "Average height")
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Part 9

``` r
line_medals <- medals %>% 
  left_join(country, by = "athlete_id") %>% 
  separate(Games.x,c("[0-9]+","[A-Za-z]")) %>% 
  rename(Year="[0-9]+", Season = "[A-Za-z]") %>% 
  mutate(Year = as.integer(Year)) %>% 
  filter(Medal %in% c("Gold","Silver","Bronze")) %>% 
  distinct(NOC,Year,Season,Medal,Sport) %>% 
  select(Year,Season,Medal) %>%
  count(Year,Medal,Season)
line_medals %>%
  mutate(Medal = factor(Medal, levels = c("Gold","Silver","Bronze"))) %>%
  ggplot(aes(Year,n,color = Medal))+
  geom_line()+
  facet_wrap(vars(Season),scales = "free")+
  scale_color_manual(values=c("#ffd700","#c0c0c0","#cd7f32"))+
  labs(title = "Number of medals given by year",
       subtitle = "In summer olympics and winter olympics",
       y = "Number of medals")+
  theme_light()
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> We can see
that the number of medals change over time.
