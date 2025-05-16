#!/bin/bash

set -e  # Exit on error

# Configuration
QUICK_MODE=${QUICK_MODE:-0}
PARALLEL_TESTS=${PARALLEL_TESTS:-1}
TEST_LOG_LEVEL=${TEST_LOG_LEVEL:-info}

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color
RED='\033[0;31m'

# Function to run tests with timing
run_tests() {
    local test_name=$1
    local test_cmd=$2
    
    echo -e "${YELLOW}Running ${test_name} tests...${NC}"
    
    # Time the test execution
    local start_time=$(date +%s)
    
    # Run the test command
    if eval "$test_cmd"; then
        local status="${GREEN}PASSED${NC}"
    else
        local status="${RED}FAILED${NC}"
    fi
    
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    echo -e "${YELLOW}${test_name} tests ${status} in ${duration} seconds${NC}"
    echo "----------------------------------------"
}

# Build the project first
echo -e "${YELLOW}Building project...${NC}"
cabal build --enable-tests

# Determine which test command to use
if [ "$QUICK_MODE" = "1" ]; then
    echo -e "${YELLOW}Running in QUICK mode${NC}"
    TEST_TARGET="haskpy-test-quick"
    TEST_ARGS="--test-option=--color --test-option=--format=progress"
else
    TEST_TARGET="haskpy-test"
    TEST_ARGS="--test-show-details=streaming --test-option=--color --test-option=--format=progress"
    
    if [ "$PARALLEL_TESTS" = "1" ]; then
        TEST_ARGS="$TEST_ARGS --test-option=--jobs=4"
    fi
fi

# Run the main test suite
run_tests "Haskell" "cabal test $TEST_TARGET $TEST_ARGS"

# Run Python tests if they exist
if [ -f "test_python_client.py" ]; then
    run_tests "Python" "python3 -m pytest -v test_python_client.py"
fi

echo -e "${GREEN}All tests completed!${NC}"
