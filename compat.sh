#!/usr/bin/env bash

LUSTRE_DIR=../lustre-v6
tot=0
ok=0

echo
echo "=== Should work ==="
echo

for f in $LUSTRE_DIR/test/should_work/*; do
    ((tot=$tot+1))
    (cargo run -- $f &> /dev/null) | rg 'Parsing: OK'
    if [ $? -eq 1 ]; then
        echo "[FAIL] $f"
    else
        echo "[OK]   $f"
        ((ok=$ok+1))
    fi
done

echo
echo "=== Should fail ==="
echo

for f in $LUSTRE_DIR/test/should_fail/*; do
    (cargo run -- $f &> /dev/null) | rg 'Partial AST'
    ((tot=$tot+1))
    if [ $? -eq 1 ]; then
        echo "[FAIL] $f"
    else
        ((ok=$ok+1))
        echo "[OK]   $f"
    fi
done

echo
echo "Total: $ok / $tot"
