#!/bin/bash
# Setup script for PCC multi-architecture testing dependencies

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[✓]${NC} $*"; }
log_error() { echo -e "${RED}[✗]${NC} $*"; }
log_warning() { echo -e "${YELLOW}[!]${NC} $*"; }

check_root() {
    if [ "$(id -u)" -ne 0 ]; then
        log_error "This script must be run as root or with sudo"
        exit 1
    fi
}

detect_distro() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        DISTRO=$ID
        log "Detected distribution: $NAME"
    else
        log_error "Cannot detect distribution"
        exit 1
    fi
}

setup_i386_arch() {
    log "Setting up i386 architecture support..."
    
    case $DISTRO in
        ubuntu|debian)
            if dpkg --print-foreign-architectures | grep -q i386; then
                log_success "i386 architecture already enabled"
            else
                log "Enabling i386 architecture..."
                dpkg --add-architecture i386
                log_success "i386 architecture enabled"
            fi
            ;;
        fedora|rhel|centos)
            log_success "i386 support built-in on Red Hat systems"
            ;;
        *)
            log_warning "Unknown distribution, skipping i386 setup"
            ;;
    esac
}

install_packages() {
    log "Installing required packages..."
    
    case $DISTRO in
        ubuntu|debian)
            apt-get update
            apt-get install -y \
                gcc-mingw-w64-x86-64 \
                gcc-mingw-w64-i686 \
                g++-mingw-w64-x86-64 \
                g++-mingw-w64-i686 \
                gcc-multilib \
                g++-multilib \
                wine \
                wine32 \
                wine64 \
                binfmt-support \
                qemu-user-static \
                file \
                binutils-mingw-w64 \
                || true
            log_success "Packages installed"
            ;;
            
        fedora)
            dnf install -y \
                mingw64-gcc \
                mingw32-gcc \
                mingw64-g++ \
                mingw32-g++ \
                wine \
                glibc-devel.i686 \
                libgcc.i686 \
                || true
            log_success "Packages installed"
            ;;
            
        arch)
            pacman -S --noconfirm \
                mingw-w64-gcc \
                wine \
                lib32-gcc-libs \
                multilib-devel \
                || true
            log_success "Packages installed"
            ;;
            
        *)
            log_error "Unsupported distribution: $DISTRO"
            log "Please install these packages manually:"
            echo "  - MinGW cross-compilers (x86_64 and i686)"
            echo "  - Wine (wine, wine32, wine64)"
            echo "  - GCC multilib support"
            exit 1
            ;;
    esac
}

configure_wine() {
    log "Configuring Wine..."
    
    # Initialize wine for the user
    if [ -n "$SUDO_USER" ]; then
        log "Initializing Wine for user $SUDO_USER..."
        su - "$SUDO_USER" -c "DISPLAY= wine wineboot --init" 2>/dev/null || true
        log_success "Wine initialized"
    else
        log_warning "Run 'wine wineboot --init' as your user to initialize Wine"
    fi
}

verify_installation() {
    log ""
    log "Verifying installation..."
    
    local all_ok=true
    
    # Check MinGW
    if command -v x86_64-w64-mingw32-gcc &> /dev/null; then
        log_success "MinGW x86_64 found: $(x86_64-w64-mingw32-gcc --version | head -1)"
    else
        log_error "MinGW x86_64 not found"
        all_ok=false
    fi
    
    if command -v i686-w64-mingw32-gcc &> /dev/null; then
        log_success "MinGW i686 found: $(i686-w64-mingw32-gcc --version | head -1)"
    else
        log_error "MinGW i686 not found"
        all_ok=false
    fi
    
    # Check Wine
    if command -v wine &> /dev/null; then
        log_success "Wine found: $(wine --version 2>/dev/null | head -1 || echo 'installed')"
    else
        log_error "Wine not found"
        all_ok=false
    fi
    
    # Check multilib
    if [ -f /usr/lib/gcc/x86_64-linux-gnu/*/32/libgcc.a ] || \
       [ -f /usr/lib/gcc/i686-linux-gnu/*/libgcc.a ]; then
        log_success "GCC multilib found"
    else
        log_warning "GCC multilib may not be properly installed"
    fi
    
    log ""
    if [ "$all_ok" = true ]; then
        log_success "All dependencies verified!"
        log ""
        log "You can now run: ./test-multiarch.sh"
    else
        log_error "Some dependencies are missing"
        log "Please check the errors above"
        return 1
    fi
}

main() {
    log "======================================"
    log "PCC Multi-Arch Dependencies Setup"
    log "======================================"
    log ""
    
    check_root
    detect_distro
    setup_i386_arch
    install_packages
    configure_wine
    verify_installation
}

main
