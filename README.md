# Vaccine Explorer

## Executive Summary

**Undoubtedly, the Affordable Care Act has improved health insurance rates across the US, but how---if at all---has it improved healthcare outcomes?**

In particular, Vaccine Explorer allows users to get a hands-on look at how Medicaid expansion under the Affordable Care Act has affected vaccination rates across in US in recent years (spoiler alert: it hasn't affected them very much).
By using Vaccine Explorer, it quickly becomes evident that Medicaid expansion has drastically improved insurance rates in the vast majority of states that have opted in. 
At the same time though, Vaccine Explorer also reveals that Medicaid expansion has done little to improve vaccination rates themselves. 
Lastly, Vaccine Explorer also uncovers that poverty and insurance rates are associated with vaccination rates, and that this association is stronger for some vaccines than others---revealing a way to measure the perceived importance of certain vaccination schedules. 


## Web Application

Vaccine Explorer can be found [here](https://rqiu.shinyapps.io/Vaccine-Explorer/).


## Key Takeaways

* States that have opted for Medicaid expansion have seen **dramatically improved insurance rates**.
* **Medicaid expansion does not seem to substantially improve vaccination rates**.
* Insurance rates weakly associated with improved vaccination rates while poverty rates weakly associated with worsened vaccination rates.
* Associations stronger in more critical vaccines like Hepatitis B, implying that **people tend to opt-out of less critical vaccines**. 
* Generally, **vaccination rates in the US have stagnated** with the exception of Rotavirus, which has seen a strong uptick in recent years.


## Background Information

One major goal of the Affordable Care Act (ACA), commonly known as "Obamacare", was to reduce the number of adults in the US without health insurance. The ACA has tried to accomplish this through two main avenues: the creation of a tightly regulated Health Insurance Marketplace and by allowing states to expand Medicaid coverage to most low-income adults. And while it has generally been accepted that the ACA *has* improved overall health insurance rates in the US, it remains ambiguous how to attribute those gains and whether or not Medicaid expansion has been beneficial. Additionally, it's difficult to determine if the ACA has actually improved healthcare outcomes directly (and if it has, how). These are the questions Vaccine Explorer hopes to answer. 



## Data Sources

Immunization data was obtained from the [CDC's National Immunization Survey's public-use data files, 2010-2016](https://www.cdc.gov/vaccines/imz-managers/nis/datasets.html).

[Historic](https://www.census.gov/data/tables/time-series/demo/health-insurance/historical-series/hic.html) and recent, Medicaid-expansion encoded data were obtained from the US Census Bureau's Health Insurance Coverage series ([2016](https://www.census.gov/library/publications/2017/demo/p60-260.html), [2017](https://www.census.gov/library/publications/2018/demo/p60-264.html)). 

[Historical state-level poverty data](https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-people.html) was obtained from the US Census Bureau.

States' [kindergarten immunization requirements](https://vaccines.procon.org/view.resource.php?resourceID=005979) were retrieved from ProCon.org.




