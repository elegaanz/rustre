#!/usr/bin/env bash

LUSTRE_DIR=../lustre-v6
BIN=rustre-ast-dump
BIN_PATH=./target/release/$BIN
tot=0
ok=0

cargo build --release --package $BIN

echo
echo "=== Checking dependencies ==="
echo

file $BIN_PATH || exit
command -v rg || (echo "ripgrep not installed" && exit)

echo
echo "=== Should work ==="
echo

for f in $(find $LUSTRE_DIR/test/should_work/ -type f); do
    ((tot=$tot+1))
    $BIN_PATH $f &> /dev/null
    if [ $? -eq 0 ]; then
        echo "[ OK ] $f"
        ((ok=$ok+1))
    else
        echo "[FAIL] $f"
    fi
done

echo
echo "=== Should fail ==="
echo
#Pour chaque test faudrait aussi faire une version où on rajoute du trivia entre tous les lexèmes non-trivia pour vérifier qu'on gère bien ça. Et à la limite un autre où on enlève le trivia qui pourrait être là (quitte à avoir une liste de lexèmes qui ne peuvent plus être serialisée vu que des idents ou kw peuvent se toucher)
for f in $(find $LUSTRE_DIR/test/should_fail/ -type f); do
    $BIN_PATH $f &> /dev/null
    ((tot=$tot+1))
    if [ $? -eq 0 ]; then
        ((ok=$ok+1))
        echo "[ OK ] $f"
    else
        echo "[FAIL] $f"
    fi
done

echo
echo "Total: $ok / $tot"
