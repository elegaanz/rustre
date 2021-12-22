#!/usr/bin/env bash

LUSTRE_DIR=../lustre-v6
tot=0
ok=0

cargo build --release

echo
echo "=== Should work ==="
echo

for f in $(find $LUSTRE_DIR/test/should_work/ -type f); do
    ((tot=$tot+1))
    (./target/release/lustrs $f | rg 'Parsing: OK') &> /dev/null
    if [ $? -eq 1 ]; then
        echo "[FAIL] $f"
    else
        echo "[ OK ] $f"
        ((ok=$ok+1))
    fi
done

echo
echo "=== Should fail ==="
echo

for f in $(find $LUSTRE_DIR/test/should_fail/ -type f); do
    (./target/release/lustrs $f | rg 'Partial AST') &> /dev/null
    ((tot=$tot+1))
    if [ $? -eq 1 ]; then
        echo "[FAIL] $f"
    else
        ((ok=$ok+1))
        echo "[ OK ] $f"
    fi
done

echo
echo "Total: $ok / $tot"
