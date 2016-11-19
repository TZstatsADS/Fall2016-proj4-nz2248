# Project: Words 4 Music

In this project, I built a training function to associate several music topics with their high frequent words. 
For the feature engineering part, I selected several statistics for each list under each h5 file, including sd, mean,length ,min,max and quantiles. So there are total 296 variables for each songs.

First I used the LDA topic modelling method to rank the words in each topic.

![screenshot](https://github.com/TZstatsADS/Fall2016-proj4-nz2248/blob/master/figs/1.pic_hd.jpg)
![screenshot](https://github.com/TZstatsADS/Fall2016-proj4-nz2248/blob/master/figs/2.pic_hd.jpg)
![screenshot](https://github.com/TZstatsADS/Fall2016-proj4-nz2248/blob/master/figs/3.pic_hd.jpg)
![screenshot](https://github.com/TZstatsADS/Fall2016-proj4-nz2248/blob/master/figs/4.pic_hd.jpg)
![screenshot](https://github.com/TZstatsADS/Fall2016-proj4-nz2248/blob/master/figs/5.pic_hd.jpg)

Then Use the LDA topics to train hd5 analysis, and finally get the words rank dictionary for each topic.

Tested the 100-song sample on the trained data and get the ranked word csv.
