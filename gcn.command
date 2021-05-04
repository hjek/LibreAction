#!/bin/bash

# Go to directory of this script
cd -- "$(dirname "$0")"

# Install Brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install SWI Prolog using Brew
brew install swi-prolog

# Run the app
swipl gcn.pl
