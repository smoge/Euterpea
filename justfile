# Default task
default: build

# Build the project
build:
    stack build

# Build the project 
build-watch:
    stack build --file-watch --fast

# Clean and rebuild the project
clean-build: clean-full build

# Test the project
test:
    stack test

# Run tests with watch mode
test-watch:
    stack test --file-watch

# Clean the project
clean:
    stack clean

# Deep clean - removes all build artifacts and dependencies
clean-full:
    stack clean --full
    rm -rf .stack-work

# Generate Haddock documentation
docs-no-show:
    stack haddock
    @echo "Documentation generated. Open .stack-work/dist/*/doc/html/*/index.html in your browser"

# Generate and open documentation in default browser
docs: docs
    @if command -v xdg-open > /dev/null; then \
        xdg-open `find .stack-work/dist -name index.html | grep -v "/doc/html/$" | head -n1`; \
    elif command -v open > /dev/null; then \
        open `find .stack-work/dist -name index.html | grep -v "/doc/html/$" | head -n1`; \
    fi

# Format Haskell project with ormolu
format-with-ormolu:
    echo "Formating the Haskell project."
    find ./src -name '*.hs' | xargs ormolu -i
    find ./test -name '*.hs' | xargs ormolu -i
    find ./bench -name '*.hs' | xargs ormolu -i

# Format Haskell project with fourmolu
format-with-fourmolu:
    echo "Formating the Haskell project."
    find ./src -name '*.hs' | xargs fourmolu -i
    find ./test -name '*.hs' | xargs fourmolu -i
    find ./bench -name '*.hs' | xargs fourmolu -i


# Bench
bench:
    stack bench --flag euterpea:bench

# Run specific benchmark (usage: just bench-suite suite-name)
bench-suite suite-name:
    stack bench :${suite-name}


