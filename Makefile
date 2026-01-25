# Makefile for Aeth
#
# This Makefile provides targets for building, installing, uninstalling,
# and managing dependencies for the Aeth Haskell project across various
# Linux distributions.
#
# Usage:
#   make help          - Display this help message.
#   make deps          - Install system-level dependencies (GHC, Cabal) for your distribution.
#   make build         - Build the Aeth project.
#   make install       - Install the Aeth executable to $(PREFIX)/bin.
#   make uninstall     - Uninstall the Aeth executable.
#   make run           - Run the Aeth project.
#   make clean         - Clean build artifacts.

.PHONY: all help deps build install uninstall run clean

# --- Configuration ---
PROJECT_NAME := Aeth
CABAL_EXE_NAME := $(strip Aeth)# As defined in aeth.cabal
PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
# DESTDIR is used by packaging tools

# --- Distribution Detection ---
# Try to detect the distribution using lsb_release or /etc/os-release
DISTRO := $(shell lsb_release -si 2>/dev/null || cat /etc/os-release | grep -E "^ID=" | cut -d'=' -f2 | tr -d '"')
# Convert to lowercase for easier matching
DISTRO := $(shell echo $(DISTRO) | tr '[:upper:]' '[:lower:]')

# --- Dependency Commands ---
ifeq ($(DISTRO),ubuntu)
    DEPS_CMD := sudo apt-get update && sudo apt-get install -y build-essential ghc cabal-install libvty-dev
else ifeq ($(DISTRO),debian)
    DEPS_CMD := sudo apt-get update && sudo apt-get install -y build-essential ghc cabal-install libvty-dev
else ifeq ($(DISTRO),arch)
    DEPS_CMD := sudo pacman -Syu --noconfirm base-devel ghc cabal-install vty
else ifeq ($(DISTRO),fedora)
    # dnf check-update returns 100 if updates are available, so we ignore the exit code
    DEPS_CMD := sudo dnf check-update || true; sudo dnf install -y @development-tools ghc cabal-install ncurses-devel
else ifeq ($(DISTRO),centos)
    DEPS_CMD := sudo dnf check-update || true; sudo dnf install -y @development-tools ghc cabal-install ncurses-devel
else ifeq ($(DISTRO),gentoo)
    DEPS_CMD := sudo emerge --noreplace dev-lang/ghc dev-haskell/cabal-install sys-libs/ncurses
else ifeq ($(DISTRO),nixos)
    DEPS_CMD := @echo "NixOS detected. For NixOS, it's recommended to define a Nix expression for your project."; \
                echo "This Makefile will not attempt to install system dependencies via Nix."; \
                echo "You might need to use 'nix-shell -p ghc cabal-install' or create a default.nix file."
else
    DEPS_CMD := @echo "Unknown distribution: $(DISTRO). Please install 'ghc' and 'cabal-install' manually."; \
                echo "You may also need development headers for ncurses or vty if you encounter build issues."
endif

# --- Targets ---

all: build

help:
	@echo "Usage:"
	@echo "  make help          - Display this help message."
	@echo "  make deps          - Install system-level dependencies (GHC, Cabal) for your distribution."
	@echo "  make build         - Build the Aeth project."
	@echo "  make install       - Install the Aeth executable to $(BINDIR)."
	@echo "  make uninstall     - Uninstall the Aeth executable."
	@echo "  make run           - Run the Aeth project."
	@echo "  make clean         - Clean build artifacts."
	@echo ""
	@echo "Detected Distribution: $(DISTRO)"
	@echo "Note: The 'deps' target requires sudo for package installation."

deps:
	@echo "Installing system dependencies for $(DISTRO)..."
	@echo "This target requires sudo permissions."
	@echo "Please enter your password if prompted."
	$(DEPS_CMD)
	@echo "Updating cabal package list..."
	cabal update
	@echo "Installing vty-unix..."
	cabal install --lib vty-unix

build:
	@echo "Building $(PROJECT_NAME)..."
	@if [ ! -d "$(HOME)/.cabal/packages/hackage.haskell.org" ]; then \
		echo "Package list not found. Running 'cabal update'..."; \
		cabal update; \
	fi
	cabal build all

install: build
	@echo "Installing $(PROJECT_NAME) to $(DESTDIR)$(BINDIR)..."
	mkdir -p "$(DESTDIR)$(BINDIR)"
	$(eval EXE_PATH := $(shell find dist-newstyle -type f -name $(CABAL_EXE_NAME) -executable | grep -v "\-tmp" | head -n 1))
	@if [ -z "$(EXE_PATH)" ]; then echo "Error: Could not find executable. Run 'make build' first."; exit 1; fi
	install -Dm755 "$(EXE_PATH)" "$(DESTDIR)$(BINDIR)/$(PROJECT_NAME)"
	@echo "Installation successful."

uninstall:
	@echo "Uninstalling $(PROJECT_NAME)..."
	rm -f "$(DESTDIR)$(BINDIR)/$(PROJECT_NAME)"
	@echo "Uninstallation successful."

run: build
	@echo "Running $(PROJECT_NAME)..."
	cabal run $(CABAL_EXE_NAME) -- $(ARGS)

clean:
	@echo "Cleaning build artifacts for $(PROJECT_NAME)..."
	cabal clean
