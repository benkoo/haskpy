# HaskPy - Haskell-Python Integration

A demonstration of integrating Haskell and Python, featuring a web service that exposes Haskell functions to be called from Python.

## Features

- **Web Service**: A lightweight HTTP server built with Scotty that exposes Haskell functions
- **Math Operations**: Basic arithmetic operations (add, subtract, multiply, divide)
- **Fibonacci Sequence**: Calculate the nth Fibonacci number
- **Python Integration**: Example Python client to call Haskell functions
- **Error Handling**: Proper error handling for invalid operations and edge cases

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler) 9.6.7 or later
- [Cabal](https://www.haskell.org/cabal/) (Haskell build tool)
- Python 3.6 or later
- Python `requests` library

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

### 2. Install Python Dependencies

```bash
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

2. Run the example:
   ```bash
   cabal run
   ```

### Example Code

See `src/Main.hs` for examples of:
- Running Python code from Haskell
- Passing values between Haskell and Python
- Using Python's standard library
- Working with Python functions and data structures

The example includes:
- Simple Python print statements
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
