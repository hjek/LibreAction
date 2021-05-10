#!/bin/bash

# Go to directory of this script
cd -- "$(dirname "$0")"

# Install Brew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install SWI Prolog using Brew (if it isn't already there)
if ! command -v swipl
	then brew install swi-prolog
fi

# Run the app
swipl libreaction.pl
