# odm-xml-to-r
Script to use when importing a dataextract in ODM XML 1.3 with full extensions into R.

This repository has as starting point two scripts that were developed by Albert Cobos.
His comments:
- 01\_read\_oc\_xml\_data.R,  which makes use of the (newer) package XML2, as well as package dplyr for dataframe manipulation and the "pipe" operator (%>%). As you will see, in this case I'm just reeding what I need. The result of running the script is a normalized dataframe (data) containing all the clinical data, plus additional dataframes for events, forms, groups, items_ item_codelists and subjects.  Once we have this, its easy to write functions to reshape the data so as to have, for instance, one dataframe for each group. This is the script I wrote for the study I'm currently working in (first time used!).

- older.R, is from a previous study, and makes use of packages XML and plyr. Here I was more thoruogh in reading everything, and as a result more dataframes are created (such as FormDef and FormRef for forms, etc). This is the script I have been using for most studies, with some adaptation for each.

An noticeable, additional difference between both scripts: in the first one, I used function xml_ns_strip() to get rid of namespaces. I don't know if this is a dangerous idea, but I just tried and worked.

