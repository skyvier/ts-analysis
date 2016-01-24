# HUnit Testing

The library utilizes to automatic HUnit tests with cabal integration. Because of the heavy focus on numerical calculations, I've decided to use a well established statistical computing tool, [R](https://www.r-project.org/), as a basis for testing the calculated values. This means that installation of the software is required if you want to run complete tests. The R side of things is managed completely in the few lines of `GenTestValues.r`. The script saves useful data to the `test/data` folder which is then used in the Haskell side of things.

This allows the tester to play around with different kinds of source data by modifying the `test/data/TestSource.csv` file. So far I've only began working on the tests for general Regression tools and expanding to the RegressionDiagnostics module will take some work.

## Running the tests

Once you've installed R through your favorite package manager, you can run the tests through cabal:
```
cabal configure --enable-tests
cabal build
cabal test
```

## Bugs
- The residualsTest doesn't seem to work and running it crashes the whole test suite.
