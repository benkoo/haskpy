import requests

# Example 1: Call math endpoint
response = requests.post('http://localhost:3000/math', 
    json={"operation": "add", "x": 10, "y": 20})
print("Add: ", response.json())

# Example 2: Call Fibonacci endpoint
response = requests.get('http://localhost:3000/fib/10')
print("10th Fibonacci number:", response.json())
