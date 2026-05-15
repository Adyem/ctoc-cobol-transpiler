#!/usr/bin/env bash

set -euo pipefail

GOINFRE_ROOT="${GOINFRE_ROOT:-/goinfre/${USER}}"
LOCAL_PREFIX="${LOCAL_PREFIX:-${GOINFRE_ROOT}/local}"
LOCAL_SRC="${LOCAL_SRC:-${GOINFRE_ROOT}/local-src}"
M4_VERSION="${M4_VERSION:-1.4.19}"
GMP_VERSION="${GMP_VERSION:-6.3.0}"
GNUCOBOL_VERSION="${GNUCOBOL_VERSION:-3.2}"
BUILD_JOBS="${BUILD_JOBS:-4}"

M4_TARBALL="m4-${M4_VERSION}.tar.xz"
GMP_TARBALL="gmp-${GMP_VERSION}.tar.xz"
GNUCOBOL_TARBALL="gnucobol-${GNUCOBOL_VERSION}.tar.xz"

M4_URL="https://ftp.gnu.org/gnu/m4/${M4_TARBALL}"
GMP_URL="https://ftp.gnu.org/gnu/gmp/${GMP_TARBALL}"
GNUCOBOL_URL="https://ftp.gnu.org/gnu/gnucobol/${GNUCOBOL_TARBALL}"

ensure_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        printf 'Missing required command: %s\n' "$1" >&2
        exit 1
    fi
}

fetch_tarball() {
    local url="$1"
    local file_name="$2"

    if [ ! -f "${LOCAL_SRC}/${file_name}" ]; then
        printf '[ctoc] downloading %s\n' "${file_name}"
        curl -L --fail --output "${LOCAL_SRC}/${file_name}" "${url}"
    fi
}

extract_tarball() {
    local file_name="$1"
    local dir_name="$2"

    if [ ! -d "${LOCAL_SRC}/${dir_name}" ]; then
        printf '[ctoc] extracting %s\n' "${file_name}"
        tar -xf "${LOCAL_SRC}/${file_name}" -C "${LOCAL_SRC}"
    fi
}

build_m4() {
    if [ -x "${LOCAL_PREFIX}/bin/m4" ]; then
        printf '[ctoc] m4 already installed at %s\n' "${LOCAL_PREFIX}/bin/m4"
        return
    fi
    fetch_tarball "${M4_URL}" "${M4_TARBALL}"
    extract_tarball "${M4_TARBALL}" "m4-${M4_VERSION}"
    cd "${LOCAL_SRC}/m4-${M4_VERSION}"
    ./configure --prefix="${LOCAL_PREFIX}"
    make -j"${BUILD_JOBS}"
    make install
}

build_gmp() {
    if [ -f "${LOCAL_PREFIX}/include/gmp.h" ] && [ -f "${LOCAL_PREFIX}/lib/libgmp.so" ]; then
        printf '[ctoc] gmp already installed at %s\n' "${LOCAL_PREFIX}"
        return
    fi
    fetch_tarball "${GMP_URL}" "${GMP_TARBALL}"
    extract_tarball "${GMP_TARBALL}" "gmp-${GMP_VERSION}"
    cd "${LOCAL_SRC}/gmp-${GMP_VERSION}"
    ./configure --prefix="${LOCAL_PREFIX}"
    make -j"${BUILD_JOBS}"
    make install
}

build_gnucobol() {
    if [ -x "${LOCAL_PREFIX}/bin/cobc" ] && "${LOCAL_PREFIX}/bin/cobc" -V 2>/dev/null | grep -q "GnuCOBOL"; then
        printf '[ctoc] gnucobol already installed at %s\n' "${LOCAL_PREFIX}/bin/cobc"
        return
    fi
    fetch_tarball "${GNUCOBOL_URL}" "${GNUCOBOL_TARBALL}"
    extract_tarball "${GNUCOBOL_TARBALL}" "gnucobol-${GNUCOBOL_VERSION}"
    export PATH="${LOCAL_PREFIX}/bin:${PATH}"
    export CPPFLAGS="-I${LOCAL_PREFIX}/include"
    export LDFLAGS="-L${LOCAL_PREFIX}/lib -L${LOCAL_PREFIX}/lib64"
    export PKG_CONFIG_PATH="${LOCAL_PREFIX}/lib/pkgconfig:${LOCAL_PREFIX}/lib64/pkgconfig"
    export LD_LIBRARY_PATH="${LOCAL_PREFIX}/lib:${LOCAL_PREFIX}/lib64${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}"
    cd "${LOCAL_SRC}/gnucobol-${GNUCOBOL_VERSION}"
    ./configure --prefix="${LOCAL_PREFIX}" --without-db
    make -j"${BUILD_JOBS}"
    make install
}

main() {
    ensure_command curl
    ensure_command tar
    ensure_command make
    ensure_command gcc
    mkdir -p "${LOCAL_SRC}" "${LOCAL_PREFIX}"
    build_m4
    export PATH="${LOCAL_PREFIX}/bin:${PATH}"
    build_gmp
    build_gnucobol
    printf '\n[ctoc] installed local toolchain in %s\n' "${LOCAL_PREFIX}"
    printf '[ctoc] make will use this toolchain automatically when cobc is not on PATH.\n'
    printf '[ctoc] for manual use in this shell, run:\n'
    printf '  export PATH=%q/bin:$PATH\n' "${LOCAL_PREFIX}"
    printf '  export LD_LIBRARY_PATH=%q/lib:%q/lib64${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\n' "${LOCAL_PREFIX}" "${LOCAL_PREFIX}"
    printf '  export CPATH=%q/include${CPATH:+:$CPATH}\n' "${LOCAL_PREFIX}"
    printf '  export LIBRARY_PATH=%q/lib:%q/lib64${LIBRARY_PATH:+:$LIBRARY_PATH}\n' "${LOCAL_PREFIX}" "${LOCAL_PREFIX}"
}

main "$@"
