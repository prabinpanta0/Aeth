# Maintainer: prabinpanta0 <me@prabinpanta.com.np>
pkgname=aeth-git
pkgver=r18.4e704c6
pkgrel=1
pkgdesc="An elegant polymorphic shell that lacks the concept of elegance. Written in Haskell."
arch=('x86_64')
url="https://github.com/prabinpanta0/Aeth"
license=('MIT')
depends=('gmp' 'ncurses' 'zlib' 'libffi')
makedepends=('ghc' 'ghc-static' 'cabal-install' 'git')
provides=('aeth')
conflicts=('aeth')
source=("${pkgname}::git+https://github.com/prabinpanta0/Aeth.git")
sha256sums=('SKIP')

pkgver() {
  cd "${srcdir}/${pkgname}"
  # Attempt to use git describe, fallback to count.hash
  ( set -o pipefail
    git describe --long 2>/dev/null | sed 's/\([^-]*-g\)/r\1/;s/-/./g' ||
    printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short HEAD)"
  )
}

build() {
  cd "${srcdir}/${pkgname}"

  # Use a local home directory for cabal to avoid messing with user's config
  mkdir -p "$srcdir/home/.cabal"
  export HOME="$srcdir/home"

  cabal update
  # Build with optimization and stripping
  # --disable-shared --enable-static ensures we build static libraries for dependencies
  # preventing linking errors against Arch's dynamic-only system Haskell packages
  cabal build --enable-executable-stripping --enable-split-sections --disable-shared --enable-static
}

package() {
  cd "${srcdir}/${pkgname}"
  
  # Specify HOME again just in case makefile invokes cabal and needs it
  export HOME="$srcdir/home"

  # Install using the project's Makefile
  make install DESTDIR="$pkgdir" PREFIX=/usr

  # Install License (Mandatory for Arch)
  install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"

  # Install Documentation and Example Config
  install -Dm644 README.md "$pkgdir/usr/share/doc/$pkgname/README.md"
  if compgen -G "docs/*.md" > /dev/null; then
    install -Dm644 docs/*.md "$pkgdir/usr/share/doc/$pkgname/"
  fi
  install -Dm644 config/config.toml "$pkgdir/usr/share/doc/$pkgname/examples/config.toml"
}
