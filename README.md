# **ngs-data**

This repo hosts NFL Next Gen Stats Data scraped from the [NGS website](https://nextgenstats.nfl.com).

Starting in 2016 the data include `passing`, `rushing` and `receiving` stats. The data are available separately by season and as a combined file. 

The data are saved in multiple formats: 

 - compressed csv (`.csv.gz`) for non-R users
 - native R serialized and compressed format (`.rds`)
 - quick serialization R format (`.qs`)
