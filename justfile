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
format:
    echo "Formating the Haskell project (ormolu)..."
    find ./Euterpea -name '*.hs' | xargs ormolu -i
    find ./test -name '*.hs' | xargs ormolu -i