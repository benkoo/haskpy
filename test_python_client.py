import pytest
import requests
import subprocess
import time
from typing import Generator, Any

# Test server URL
BASE_URL = "http://localhost:3000"

# Fixture to start and stop the server
@pytest.fixture(scope="module")
def server() -> Generator[None, None, None]:
    # Start the server in a separate process
    process = subprocess.Popen(
        ["cabal", "run", "haskpy"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )
    
    # Give the server some time to start
    time.sleep(2)
    
    yield  # This is where the testing happens
    
    # Cleanup: stop the server
    process.terminate()
    try:
        process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        process.kill()

def test_server_health(server: Any) -> None:
    """Test that the server is running and responding."""
    response = requests.get(f"{BASE_URL}/")
    assert response.status_code == 200
    assert "Haskell Web Service" in response.text

def test_math_add(server: Any) -> None:
    """Test the add operation."""
    response = requests.post(
        f"{BASE_URL}/math",
        json={"operation": "add", "x": 10, "y": 20}
    )
    assert response.status_code == 200
    data = response.json()
    assert data["result"] == 30.0
    assert data["statusMsg"] == "success"

def test_math_subtract(server: Any) -> None:
    """Test the subtract operation."""
    response = requests.post(
        f"{BASE_URL}/math",
        json={"operation": "subtract", "x": 20, "y": 10}
    )
    assert response.status_code == 200
    data = response.json()
    assert data["result"] == 10.0
    assert data["statusMsg"] == "success"

def test_math_multiply(server: Any) -> None:
    """Test the multiply operation."""
    response = requests.post(
        f"{BASE_URL}/math",
        json={"operation": "multiply", "x": 5, "y": 4}
    )
    assert response.status_code == 200
    data = response.json()
    assert data["result"] == 20.0
    assert data["statusMsg"] == "success"

def test_math_divide(server: Any) -> None:
    """Test the divide operation."""
    response = requests.post(
        f"{BASE_URL}/math",
        json={"operation": "divide", "x": 10, "y": 2}
    )
    assert response.status_code == 200
    data = response.json()
    assert data["result"] == 5.0
    assert data["statusMsg"] == "success"

def test_math_divide_by_zero(server: Any) -> None:
    """Test division by zero."""
    response = requests.post(
        f"{BASE_URL}/math",
        json={"operation": "divide", "x": 10, "y": 0}
    )
    assert response.status_code == 400
    data = response.json()
    assert data["statusMsg"] == "Invalid operation or division by zero"

def test_fibonacci(server: Any) -> None:
    """Test the Fibonacci endpoint."""
    response = requests.get(f"{BASE_URL}/fib/10")
    assert response.status_code == 200
    data = response.json()
    assert data["result"] == 55
    assert data["statusMsg"] == "success"

def test_fibonacci_negative(server: Any) -> None:
    """Test Fibonacci with negative input."""
    response = requests.get(f"{BASE_URL}/fib/-1")
    assert response.status_code == 400
    data = response.json()
    assert "must be non-negative" in data["statusMsg"]

# Run the tests
if __name__ == "__main__":
    pytest.main(["-v"])
