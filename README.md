### Time series analysis and forecasting library

# How to build?
1. Clone the repository.
2. Initialize the matrix submodule:
```
cd ts-analysis
git submodule init
git submodule update
```
3. Initialize the sandbox and add the extended matrix library as source:
```
cabal sandbox init
cabal sandbox add-source matrix
```
4. Install the library:
```
cabal install
```
5. Contribute!
