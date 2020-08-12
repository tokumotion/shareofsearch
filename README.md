# Measure your brand's share of search (SoS)

This small script will help you calculate monthly SoS of your brand. This script follows James Hankins ([@JCPHankins](https://twitter.com/JCPHankins)) methodology on the subject.

The output of the script is a dataframe with the expected values of you share of search and a graphic that looks like this.

![Evolutivo SOS Supermercados](https://i.ibb.co/bLgfLq4/Rplot03.png)

Further research and methodology improvements will follow. This project may render an R package for practitioners for work with in the future.

<b>Bibliography:</b> https://theeqplanner.wordpress.com/2020/08/07/the-most-important-metric-youve-never-heard-of/

Anyone willing to expand this script into a bigger project is welcome to fork it.

#### Libraries required:
- tidyverse
- gtrendsR
- zoo
- ggplot2
- maggritr

#### Optional libraries (for beautifying purposes):
- scales
- ggthemes

Any issues with the script, please contact me at [@tokumotion](https://twitter.com/Tokumotion)

#### On backlog:
- Creating a wrapper function for running SOS analysis.
