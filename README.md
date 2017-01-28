# whitehouse

Scripts to scrape and analyze data from whitehouse.gov, comparing pre- and post-inauguration versions in 2017.

To begin, use ```wget``` to download the data you want to analyze. Create a folder for the archive you want to download, and navigate to that folder in your terminal. Then for the current whitehouse.gov content, use:

```wget -r -e robots=off --convert-links -nd https://www.whitehouse.gov```

For past whitehouse.gov snapshots, locate the snapshot in question via the [Internet Archive Wayback Machine](https://archive.org/web/). Once you have chosen your snapshot, I recommend using the [Wayback Machine Downloader](https://github.com/hartator/wayback-machine-downloader), a Ruby gem, to download it. (See linked page for installation instructions.) With Wayback Machine Downloader installed, the following command will download a complete snapshot into the current directory. Note: this will likely take a *long* time, and the time code following the ```-t``` will be different, depending on the snapshot you've chosen. (This time code comes from the middle of the URL for the snapshot, in this case: http://web.archive.org/web/20170120112330/https://www.whitehouse.gov/ has a time code of 20170120112330.)

```wayback_machine_downloader -d ./ -t 20170120112330 https://www.whitehouse.gov```

Once you've downloaded your archives, move all the html files to their own folder. Using wget, most of them have no ```.html``` extension, so be careful! I found it helpful to make folders for ```css```, ```docs```, ```fonts```, ```html```, ```images```, and ```scripts```, to make sure I didn't miss anything.

Then open ```make_the_soup.py``` and update the ```source_folder``` and ```output_file``` variables, if necessary.

Finally, run ```python make_the_soup.py``` to convert all of the downloaded html data into a single CSV file you can use for data analysis.

## files

```make_the_soup.py``` is a Python script that takes downloaded html files, parses them for page title and content (looks for sections with id='page', the current format for whitehouse.gov pages), and saves them in a single CSV file with a single record for each page.

```mine_the_text.R``` is an R script that imports scraped and pre-processed data into R for text mining, analysis, and visualization. Currently it only does basic word, bigram, and trigram counts, and does comparative analysis between the current whitehouse.gov and [an archive of Trump campaign speeches](https://github.com/kshaffer/trump_speeches) (not included in this repository until I investigate their sources/exhaustiveness).

```trump-20170125.csv``` is an example output, based on a download of whitehouse.gov on January 25, 2017.

```obama-20170120.csv``` is an example output, based on a download of a whitehouse.gov snapshot from right before the inauguration on January 20, 2017.

```visualizations``` contains visualizations produced by the R script from preliminary analyses comparing January 20 and January 25 versions of whitehouse.gov. (Note: work still needs to be done to clean the various headers and sidebars containing site architectural information from the pages in order to better focus on the actual content of each page.)