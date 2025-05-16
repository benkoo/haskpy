#!/bin/bash

# Build the project
cabal build

# Run the tests
cabal test --test-show-details=streaming

# Run the Python client tests
python3 -m pytest -v test_python_client.py
