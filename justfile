# Default task
default: build

# Build the project
build:
    stack build

# Build the project 
build-watch:
    stack build --file-watch --fast

# Test the project
test:
    stack test

# Clean the project
clean:
    stack clean --full

# Generate documentation
docs:
    stack haddock

# Format Haskell project with ormolu
format-with-ormolu:
    echo "Formating the Haskell project."
    find ./src -name '*.hs' | xargs ormolu -i
    find ./test -name '*.hs' | xargs ormolu -i
    find ./bench -name '*.hs' | xargs ormolu -i

# Format Haskell project with formolu
format-with-fourmolu:
    echo "Formating the Haskell project."
    find ./src -name '*.hs' | xargs fourmolu -i
    find ./test -name '*.hs' | xargs fourmolu -i
    find ./bench -name '*.hs' | xargs fourmolu -i


# Bench
bench:
    stack bench --flag euterpea:bench
