const mina = require("../../../../_build/default/src/app/client_sdk/client_sdk.bc.js").minaSDK;
const fs = require('fs');

// hash 

console.log("testing hashing:")
console.log(mina.hash("hey"))

// hashFieldElems

let rawdata = fs.readFileSync('src/lib/random_oracle/test_vectors/three_wire.json');
let test_vectors = JSON.parse(rawdata);

for (test_vector of test_vectors) {
    console.log("expected output:", test_vector.output)
    const input = test_vector.input.map(hexString =>
        Uint8Array.from(Buffer.from(hexString, 'hex'))
    );
    console.log("output optained:", mina.hashFieldElems(input))
    console.log("---")
}
