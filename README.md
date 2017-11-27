STAT545-hw-Ganelin-Ilya
=======================

Owner: IG
---------

Date: 26/11/17
--------------

<!-- README.md is generated from README.Rmd. Please edit that file -->
The goal of powers is to practice functions created in the class and create a few more useful functoins. the fuctions also performs a test if the input is actual character or not.

#### Input:

-   png file name.

#### Data

-   As a data we used the angry.png file for test purposes. the actual image is saved in teh test folder.

### Example

-   This is a basic example which shows you how to solve a common problem:

``` r
## basic example code that saves luma component from the RGB png file
rgb2luma("img.png")
```

``` r
## basic example code that saves chroma1 component from the RGB png file
rgb2chroma1("img.png")
```

``` r
## basic example code that saves chroma2 component from the RGB png file
rgb2chroma2("img.png")
```

``` r
## basic example code that saves grey image out of RGB png file
rgb2grey("img.png")
```

### Notes:

I had a lot of problems with including png package into the project. I literally spent ours on that, read everything on the net, but at the end decided that the user will have other means, so I followed the advice from VIncenzo in class and used only base functions. Another problem was with the save img.rda file that I wanted to use as a default, but found no documentation what so ever on how to do it. I tried a few approaches, but it didn't work as well. So the final version is relying only on the base function and the example is a simple instruction on how to use it. I did however included the data file.
