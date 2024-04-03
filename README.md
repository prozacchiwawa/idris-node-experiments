Build and install all deps (not idris-node-example)
in idris-node-example

    npm install
    idris2 --cg javascript --build idris-node-example.ipkg
    node build/exec/node-test &

connect with wscat: 

    wscat -s count-proto --connect ws://localhost:8080
    > {"increment": 1}
    < {"count": 1}
    > {"increment": 2}
    < {"count": 3}
