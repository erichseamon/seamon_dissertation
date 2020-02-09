## Erich Seamon Dissertation Supplementary Materials. "Agricultural Insurance Loss and Relationships to Climate Across the Inland Pacific Northwest Region of the United States" ##

## University of Idaho, College of Natural Resources

The included folders contain analysis code, Rmarkdown, and data for reproducibility.

- /data.  The data folder contains all data.  In order to reproduce analyses, the steps are:

1. clone repo
2. Run the Rmarkdown appendices (A, B, or C), located in the /appendices folder.  Running each appendix will download the seamon_dissertation_dataload.R file and place it in /tmp/seamon.  Then this script is run within the appendix code to download all data and place the needed datasets into your /tmp/seamon folder.  The data is removed at the end of each appendix markdown.

NOTE:  If you wish to examine the data directly, you can run the seamon_dissertation_dataload.R file directly.  The file is within the /data folder.

- /appendices.  Contains Rmarkdown and html that re-create appendix analyses that refer to particular chapters.
- /code. modular code that performs analysis and modeling.

- NOTE:  Each folder (/data, /appendices, /code) have individual READMEs which describe folder contents.

- If you have concerns or issues, please contact erich at: erichs@uidaho.edu or erich@erich.io
