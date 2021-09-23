COMMIT="0eab29bb46449d45be14df98ce38cbb8f9a05918"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd -P)"
TMP_DIRECTORY=$(mktemp -d)
cd ${TMP_DIRECTORY}
wget https://github.com/supranational/blst/archive/${COMMIT}.zip
unzip ${COMMIT}.zip
cd blst-${COMMIT}
diff -qr . $SCRIPT_DIR/../src/blst/libblst
