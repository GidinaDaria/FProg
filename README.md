# FProg

ФПрог, №1

## Author
```bash
Кирвель Евгений, группа 151005
```

## Параметры
```bash
main String [-o|--output String] 
	  [-d|--delimiter CHAR]
          [-f|--skipFirstColumn BOOL] 
	  [-l|--skipLastColumn BOOL]
          [-z|--skipFirstLane BOOL]
	  [-p|--precision DOUBLE]  
          [-c|--clusterCount INT]
          [-m|--metric String]
          [-i|--initialMatrix String]


  -h,--help                         Show help text
  FILE                              Input file
  -o,--output                	    Output file
  -d,--delimiter                    Delimiter of csv file(default ',')
  -f --skipFirstColumn              True - skip file header(default false)
  -f,--skipLastColumn               True - skip file first column(default false)
  -z,--skipFirstLane                True - skip file last column(default true)
  -c,--clusterNumber                Clusters count (default 2)
  -p,--precision                    Precision(default 0.001)
  -m,--distanceMetric          	    Metric dunction "Hamming" or "Euclide", (default "Euclide")
  -i,--initializationMethod	    Initial matrix "RandomMatrix" or "RandomCenter"(default RandomMatrix)

```

## Installation
```bash
$ git clone https://github.com/eugenekirvel/FProg.git	
$ cabal update
$ cabal install cassava
$ cabal install HUnit
$ cabal install optparse-applicative
$ cabal configure
$ cabal build
```
#### How to use
```bash
$ dist\build\Lab1\Lab1.exe ./files/butterfly.txt -c 2
```

### Result example
```
[0.9254099117480523,7.45900882519476e-2]
[0.8276059742215462,0.1723940257784538]
[0.8981731942330619,0.10182680576693823]
[0.8485020663044136,0.1514979336955864]
[0.9778166783742333,2.2183321625766864e-2]
[0.8954182575360993,0.10458174246390069]
[0.837223419780054,0.16277658021994607]
[0.22239123105921413,0.7776087689407859]
[0.12199239729579774,0.8780076027042023]
[0.17871195362715384,0.8212880463728462]
[0.11744738086537057,0.8825526191346295]
[2.351180395338541e-2,0.9764881960466146]
[7.057196779079775e-2,0.9294280322092023]
[0.18587827398618895,0.814121726013811]
[0.15962891090737888,0.8403710890926213]
```

#### Test usage
```bash
$ dist\build\Lab1\Test.exe
```

