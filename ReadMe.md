# Tests for rco: The Code Optimizer

## Background

The basis for designing my column extraction optimiser can be seen in the microbenchmark plot of different column extraction method

![microbenchmarking different methods](https://github.com/hinduBale/TextAnalysisBasics/blob/master/basis.PNG)

## Easy Test :

Check out my easy test solution [here](https://hindubale.github.io/rco_test/easy_test)

## Medium Test:

To successfully run/check the working of the [column optimiser](https://rpubs.com/IACCancu/462502), head on to the [Medium Test folder](https://github.com/hinduBale/rco_test/tree/master/Medium%20Test) and
first run the parse.R script and then run the column_extractor.R script

### Input

![Input](https://github.com/hinduBale/rco_test/blob/master/Medium%20Test/input.PNG)

### Output


![Output](https://github.com/hinduBale/rco_test/blob/master/Medium%20Test/output.PNG)

## Hard Test
 
 ![Travis Build Successful Snippet](https://github.com/hinduBale/rco_test/blob/master/Hard_Test/travis.PNG)
 
 Check out my [Hard Test solutions](https://github.com/hinduBale/rco). I've added a column optimiser named opt_column_extractor.R in the R folder, it's tests and also a RMarkdown vignette. Since, it was passing all build tests, I've opened a [PR at the official rco repo.](https://github.com/jcrodriguez1989/rco/pull/147)
