CMD_INTERP_NATIVE="../interpreter/_build/default/bin/main.exe"
CMD_INTERP_BYTE="ocamlrun ../interpreter/_build/default/bin/main.bc"

CMD=$CMD_INTERP_BYTE

cd tests
for x in *.phx; do
    $CMD $x > tmp
    if ! diff tmp $(echo $x | sed 's/phx/out/'); then
        echo "Occured in $x"
        echo ""
    fi
done
rm tmp
