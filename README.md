# Course Project -- Getting and Cleaning Data

This repository contains the files for submission to the Course Project:

- **README.Rmd**, **README.md** -- this file itself. The Markdown (.md) is generated using the `keep_md: yes` directive in RMarkdown.
- **CodeBook.Rmd**, **CodeBook.md** -- the code book for the variables in the tidy data set. Also describes the steps used to derive the tidy data. The Markdown (.md) is generated using the `keep_md: yes` directive in RMarkdown.
- **fetch_raw_data.R** -- R script with functions that download and extract the raw data.
- **run_analysis.R** -- R script that combines, cleans and produce the tidy data set.

To produce the tidy data output, go to an appropriate working directory, and then run:


```r
source("fetch_raw_data.R")
FetchRawData()
source("run_analysis.R")
```

The scripts will create 2 data objects in your R environment:

- **combined** -- the wide data set with combined data from the training and test groups.
- **tidy** -- the smaller data set with the means of mean or std variables by activity and subject.

The raw data will be left in the ./raw directory.

The tidy data will be exported to the ./tidy/tidy.txt file.

The scripts are written and tested on Windows 7:


```r
R.Version()[c("platform", "version.string")]
```

```
## $platform
## [1] "x86_64-w64-mingw32"
## 
## $version.string
## [1] "R version 3.1.2 (2014-10-31)"
```
