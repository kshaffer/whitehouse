# whitehouse

Scripts to scrape and analyze data from whitehouse.gov, comparing pre- and post-inauguration versions in 2017.

To begin, use ```wget``` to download the data you want to analyze. Create a folder for the archive you want to download, and navigate to that folder in your terminal. Then for the current whitehouse.gov content, use:

```wget -r -e robots=off --convert-links -nd https://www.whitehouse.gov```

For past whitehouse.gov snapshots, locate the snapshot in question via the [Internet Archive Wayback Machine](https://archive.org/web/). Once you have chosen your snapshot, use the following to download it. (This will likely take a *long* time.) The time code between ```/web/``` and ```/https://``` will be different, depending on the snapshot you've chosen.

```wget -r -e robots=off --convert-links -nd http://web.archive.org/web/201701200112330/https://www.whitehouse.gov```

Once you've downloaded your archives, move all the html files to their own folder. Most of them have no ```.html``` extension, so be careful! I found it helpful to make folders for ```css```, ```docs```, ```fonts```, ```html```, ```images```, and ```scripts```, to make sure I didn't miss anything.

Then open ```make_the_soup.py``` and update the ```source_folder``` and ```output_file``` variables, if necessary.

Finally, run ```python make_the_soup.py``` to convert all of the downloaded html data into a single CSV file you can use for data analysis.
