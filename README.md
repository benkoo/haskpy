# HaskPy - Haskell-Python Integration

[![Haskell CI](https://github.com/benkoo/haskpy/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/benkoo/haskpy/actions/workflows/haskell-ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A demonstration of integrating Haskell and Python, featuring a web service that exposes Haskell functions to be called from Python.

## Features

- **Web Service**: A lightweight HTTP server built with Scotty that exposes Haskell functions
- **Math Operations**: Basic arithmetic operations (add, subtract, multiply, divide)
- **Optimized Fibonacci Algorithm**: Calculate the nth Fibonacci number using O(log n) matrix exponentiation
- **Python Integration**: Example Python client to call Haskell functions
- **Robust Error Handling**: Standardized error handling for invalid operations and edge cases
- **Precision-aware Division**: Proper handling of floating-point division with special cases
- **RESTful API**: Simple and intuitive API endpoints
- **Cross-Platform**: Works on macOS, Windows, and Linux

## Table of Contents

- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Usage](#usage)
- [API Reference](#api-reference)
- [Development](#development)
- [Contributing](#contributing)
- [License](#license)

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler) 9.6.7 or later
- [Cabal](https://www.haskell.org/cabal/) (Haskell build tool)
- Python 3.6 or later
- Python `requests` library (for the example client)

## Installation

### 1. Install Haskell Tools

#### macOS (using GHCup - Recommended)

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Follow the on-screen instructions to install GHC, Cabal, and HLS (Haskell Language Server).

#### Windows

1. Download and run the GHCup installer from: https://www.haskell.org/ghcup/
2. During installation, select to install GHC, Cabal, and HLS
3. Open a new terminal and verify the installation:
   ```
   ghc --version
   cabal --version
   ```

#### Linux (Ubuntu/Debian)

```bash
sudo apt-get update
sudo apt-get install -y haskell-platform
```

### 2. Clone the Repository

```bash
git clone https://github.com/benkoo/haskpy.git
cd haskpy
```

### 3. Install Python Dependencies

```bash
pip install -r requirements.txt
```

## Usage

### Starting the Server

```bash
cabal run
```

The server will start on `http://localhost:3000`

### Using the Python Client

```python
import requests

# Example 1: Call math endpoint
response = requests.post('http://localhost:3000/math', 
    json={"operation": "add", "x": 10, "y": 20})
print("Add: ", response.json())

# Example 2: Call Fibonacci endpoint
response = requests.get('http://localhost:3000/fib/10')
print("10th Fibonacci number:", response.json())
```

## API Reference

### POST /math

Perform a mathematical operation.

**Request Body:**
```json
{
  "operation": "add|subtract|multiply|divide",
  "x": number,
  "y": number
}
```

**Example Success Response:**
```json
{
  "result": 30,
  "statusMsg": "success"
}
```

**Example Error Response:**
```json
{
  "result": 0,
  "statusMsg": "Invalid operation or division by zero"
}
```

### GET /fib/:n

Get the nth Fibonacci number using an optimized O(log n) algorithm.

**Parameters:**
- `n`: The index of the Fibonacci number to retrieve (non-negative integer)

**Example Success Response:**
```json
{
  "result": 55,
  "statusMsg": "success"
}
```

**Example Error Response:**
```json
{
  "result": 0,
  "statusMsg": "n must be non-negative"
}
```

**Performance Note:**
The Fibonacci implementation uses matrix exponentiation for efficiency, allowing calculation of large Fibonacci numbers (up to F(10000) and beyond) in milliseconds.

## Development

### Recent Improvements

- **Optimized Fibonacci Algorithm**: Replaced recursive implementation with O(log n) matrix exponentiation
- **Floating-Point Precision**: Enhanced division operation with epsilon checks for near-zero values
- **Standardized Error Handling**: Unified error messages across all API endpoints for consistency
- **Test Improvements**: Added approximate equality tests for floating-point operations

### Building the Project

```bash
cabal build
```

### Running Tests

There are two types of tests:

1. **Haskell Unit Tests**: Test the core functionality
2. **Python Integration Tests**: Test the web API

To run all tests:

```bash
./run-tests.sh
```

#### Running Haskell Tests Only

```bash
cabal test --test-show-details=streaming
```

#### Running Python Tests Only

First, make sure the server is running:

```bash
cabal run
```

Then in a separate terminal:

```bash
pytest -v test_python_client.py
```

### Code Formatting

```bash
fourmolu -i src/**/*.hs
```

### Test Coverage

To generate a test coverage report:

```bash
cabal configure --enable-tests --enable-coverage
cabal test --test-show-details=streaming
hpc report haskpy-tests
```

## Contributing

Contributions are welcome! Please read our [Contributing Guidelines](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Scotty](https://hackage.haskell.org/package/scotty) - A Haskell web framework
- [Aeson](https://hackage.haskell.org/package/aeson) - Fast JSON parsing and encoding
- [Stack](https://docs.haskellstack.org/) - The Haskell Tool Stack
pip install requests
```

## Project Structure

```
haskpy/
├── src/
│   └── Main.hs           # Main application code
├── python_client.py      # Example Python client
├── haskpy.cabal         # Project configuration
└── README.md             # This file
```

## Building and Running

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd haskell-project
   ```

2. Build the project:
   ```bash
   cabal build
   ```

3. Run the server:
   ```bash
   cabal run
   ```

   The server will start on `http://localhost:3000`

4. In a separate terminal, run the Python client:
   ```bash
   python python_client.py
   ```

## API Endpoints

### 1. Math Operations

- **Endpoint**: `POST /math`
- **Content-Type**: `application/json`
- **Request Body**:
  ```json
  {
    "operation": "add|subtract|multiply|divide",
    "x": number,
    "y": number
  }
  ```
- **Example**:
  ```bash
  curl -X POST http://localhost:3000/math \
       -H "Content-Type: application/json" \
       -d '{"operation":"add", "x":10, "y":20}'
  ```
  **Response**:
  ```json
  {"result": 30, "statusMsg": "success"}
  ```

### 2. Fibonacci Sequence

- **Endpoint**: `GET /fib/:n`
- **Example**:
  ```bash
  curl http://localhost:3000/fib/10
  ```
  **Response**:
  ```json
  {"result": 55, "statusMsg": "success"}
  ```

## Example Python Client

The `python_client.py` file contains a simple example of how to call the Haskell web service from Python:

```python
import requests

# Example 1: Call math endpoint
response = requests.post('http://localhost:3000/math', 
    json={"operation": "add", "x": 10, "y": 20})
print("Add: ", response.json())

# Example 2: Call Fibonacci endpoint
response = requests.get('http://localhost:3000/fib/10')
print("10th Fibonacci number:", response.json())
```

## Error Handling

The API returns appropriate HTTP status codes and error messages for various error conditions:
- `400 Bad Request`: Invalid operation or parameters
- `404 Not Found`: Invalid endpoint
- `500 Internal Server Error`: Server-side error

## Dependencies

### Haskell Dependencies
- base >= 4.17
- scotty >= 0.12
- aeson >= 2.0
- http-types >= 0.12
- wai >= 3.2
- wai-extra >= 3.1
- transformers >= 0.5.0.0
- process >= 1.6.0
- directory >= 1.3.0.0

### Python Dependencies
- requests

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

#### Using GHCup (Recommended)

1. Install required system dependencies:
   ```bash
   sudo apt-get update
   sudo apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
   ```

2. Install GHCup:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```

3. Follow the on-screen prompts to install GHC, Cabal, and HLS.

4. Add GHCup to your shell configuration:
   ```bash
   echo '[ -f "${GHCUP_INSTALL_BASE:=$HOME/.ghcup}/env" ] && source "${GHCUP_INSTALL_BASE:=$HOME/.ghcup}/env"' >> ~/.bashrc  # or ~/.zshrc
   source ~/.bashrc  # or ~/.zshrc
   ```

## Building and Running the Project

1. Clone this repository (if you haven't already):
   ```bash
   git clone <repository-url>
   cd haskell-project
   ```

2. Update the package list (first time only):
   ```bash
   cabal update
   ```

3. Build the project:
   ```bash
   cabal build
   ```

4. Run the program:
   ```bash
   cabal run
   ```

## Project Structure

- `src/` - Contains the Haskell source code
  - `Main.hs` - Entry point of the application
- `haskpy.cabal` - Project configuration and dependency management
- `cabal.project` - Cabal project file

## Python Integration Example

This project demonstrates how to call Python code from Haskell using the `process` library. The example includes:

1. Running Python code from Haskell
2. Passing values between Haskell and Python
3. Using Python's standard library
4. Working with Python functions and data structures

### Prerequisites

1. Python 3.x must be installed and available in your PATH as `python3`
2. No additional Python development files are required

To check your Python installation:
```bash
python3 --version
which python3
```

### Building and Running

1. Build the project:
   ```bash
   cabal update
   cabal build
   ```

2. Run the server:
   ```bash
   cabal run
   ```
   This will start the web server on `http://localhost:3000`

## API Endpoints

### Math Operations

**POST /math**
- Performs basic arithmetic operations
- Request body:
  ```json
  {
    "operation": "add|subtract|multiply|divide",
    "x": number,
    "y": number
  }
  ```
- Example response:
  ```json
  {
    "result": 30,
    "statusMsg": "success"
  }
  ```

### Fibonacci Sequence

**GET /fib/:n**
- Returns the nth Fibonacci number
- Example: `GET /fib/10` returns:
  ```json
  {
    "result": 55,
    "statusMsg": "success"
  }
  ```

## Development

### Project Structure

```
haskpy/
├── src/                  # Haskell source code
│   ├── HaskPy/
│   │   └── Math.hs     # Math functions
│   └── Main.hs           # Web server and main application
├── test/                # Test files
│   ├── APISpec.hs       # API endpoint tests
│   ├── MathSpec.hs      # Unit tests for math functions
│   └── Spec.hs          # Test runner
├── test_python_client.py # Python client tests
├── haskpy.cabal         # Project configuration
└── cabal.project        # Cabal project settings
```

### Running Tests

There are two types of tests:

1. **Haskell Unit Tests**: Test the core functionality
2. **Python Integration Tests**: Test the web API

To run all tests:

```bash
./run-tests.sh
```

#### Running Haskell Tests Only

```bash
cabal test --test-show-details=streaming
```

#### Running Python Tests Only

First, make sure the server is running:

```bash
cabal run
```

Then in a separate terminal:

```bash
pytest -v test_python_client.py
```

### Code Formatting

```bash
fourmolu -i src/**/*.hs
```

### Test Coverage

To generate a test coverage report:

```bash
cabal configure --enable-tests --enable-coverage
cabal test --test-show-details=streaming
hpc report haskpy-tests
```
- Math operations using Python's math library
- String manipulation
- List comprehensions
- Lambda functions

## Dependencies

This project depends on:
- `base` (included with GHC)
- `process` (included with GHC)
- `text` (included with GHC)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
