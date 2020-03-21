# Tests for rco: The Code Optimizer

# Solution Number 1

## Background :

The basis for designing my column extraction optimiser can be seen in the microbenchmark plot of different column extraction method

![microbenchmarking different methods](https://github.com/hinduBale/TextAnalysisBasics/blob/master/basis.PNG)

## Easy Test :

Check out my easy test solution [here](https://hindubale.github.io/rco_test/easy_test)

## Medium Test :

To successfully run/check the working of the [column optimiser](https://rpubs.com/IACCancu/462502), head on to the [Medium Test folder](https://github.com/hinduBale/rco_test/tree/master/Medium%20Test) and
first run the parse.R script and then run the column_extractor.R script

### Input

![Input](https://github.com/hinduBale/rco_test/blob/master/Medium%20Test/input.PNG)

### Output


![Output](https://github.com/hinduBale/rco_test/blob/master/Medium%20Test/output.PNG)

## Hard Test :
 
 ![Travis Build Successful Snippet](https://github.com/hinduBale/rco_test/blob/master/Hard_Test/travis.PNG)
 
 Check out my [Hard Test solutions](https://github.com/hinduBale/rco). I've added a column optimiser named opt_column_extractor.R in the R folder, it's tests and also a RMarkdown vignette. Since, it was passing all build tests, I've opened a [PR at the official rco repo.](https://github.com/jcrodriguez1989/rco/pull/147)



# Solution Number 2

## Background

The basis for designing my value extraction optimiser can be seen in the microbenchmark plot of different value extraction method 

![Microbenchmarking of different value extraction methods](https://github.com/hinduBale/rco_test/blob/master/Capture.PNG)


## Easy Test :

Check out my easy test solution [here](https://hindubale.github.io/rco_test/easy_test)

## Medium Test :

To successfully run/check the working of the [value  optimiser](https://rpubs.com/IACCancu/462501), head on to the [Medium Test folder](https://github.com/hinduBale/rco_test/tree/master/Medium%20Test%20-%202) and
first run the parse.R script and then run the value_extractor.R script

### Input

![Input](https://github.com/hinduBale/rco_test/blob/master/Medium%20Test%20-%202/input.PNG)

### Output


![Output](https://github.com/hinduBale/rco_test/blob/master/Medium%20Test%20-%202/output.PNG)

## Hard Test :
 
 ![Travis Build Successful Snippet](https://github.com/hinduBale/rco_test/blob/master/Hard_Test%20-%202/output.PNG)
 
 Check out my [Hard Test solutions](https://github.com/hinduBale/rco/tree/value_optimiser). I've added a value optimiser named opt_value_extractor.R in the R folder, it's tests and also a RMarkdown vignette. Since, it was passing all build tests, I've opened a [PR at the official rco repo.](https://github.com/jcrodriguez1989/rco/pull/149)
