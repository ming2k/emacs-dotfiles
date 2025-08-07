#!/usr/bin/env python3
"""
Simple test script for Python development with Emacs configuration.
"""

def fibonacci(n):
    """Generate fibonacci sequence up to n terms."""
    if n <= 0:
        return []
    elif n == 1:
        return [0]
    elif n == 2:
        return [0, 1]
    
    fib = [0, 1]
    for i in range(2, n):
        fib.append(fib[i-1] + fib[i-2])
    
    return fib

def main():
    """Main function to test Python features."""
    # Test basic functionality
    numbers = fibonacci(10)
    print(f"Fibonacci sequence: {numbers}")
    
    # Test list comprehension
    squares = [x**2 for x in range(1, 6)]
    print(f"Squares: {squares}")
    
    # Test dictionary operations
    data = {
        'name': 'Emacs Configuration Test',
        'version': '1.0.0',
        'features': ['corfu', 'eglot', 'flymake', 'orderless']
    }
    
    for key, value in data.items():
        print(f"{key}: {value}")

if __name__ == "__main__":
    main()