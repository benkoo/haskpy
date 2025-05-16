# Contributing to HaskPy

First off, thanks for taking the time to contribute! :tada:

The following is a set of guidelines for contributing to HaskPy. These are just guidelines, not rules. Use your best judgment, and feel free to propose changes to this document in a pull request.

## Code of Conduct

This project and everyone participating in it is governed by our [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code.

## How Can I Contribute?

### Reporting Bugs

- **Ensure the bug was not already reported** by searching on GitHub under [Issues](https://github.com/benkoo/haskpy/issues).
- If you're unable to find an open issue addressing the problem, [open a new one](https://github.com/benkoo/haskpy/issues/new). Be sure to include:
  - A clear and descriptive title
  - A description of the expected behavior
  - A description of the actual behavior
  - Steps to reproduce the behavior
  - Any relevant logs or screenshots

### Suggesting Enhancements

- Use GitHub issues to suggest enhancements
- Clearly describe the enhancement and why it would be useful
- Include any relevant links or screenshots

### Pull Requests

1. Fork the repository and create your branch from `main`.
2. If you've added code that should be tested, add tests.
3. If you've changed APIs, update the documentation.
4. Ensure the test suite passes.
5. Make sure your code is properly formatted.
6. Issue that pull request!

## Development Setup

### Prerequisites

- GHC 9.6.7 or later
- Cabal 3.0 or later
- Python 3.6 or later

### Building

```bash
cabal build
```

### Running Tests

```bash
cabal test
```

### Code Style

- Follow the [Haskell Style Guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)
- Use `fourmolu` for code formatting

## License

By contributing to HaskPy, you agree that your contributions will be licensed under its MIT License.
