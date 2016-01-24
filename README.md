# Time series analysis and forecasting library

## How to build?
- Clone the repository.
- Initialize the matrix submodule:
```
cd ts-analysis
git submodule init
git submodule update
```
- Initialize the sandbox and add the extended matrix library as source:
```
cabal sandbox init
cabal sandbox add-source matrix
```
- Install the library:
```
cabal install
```
- Contribute!
