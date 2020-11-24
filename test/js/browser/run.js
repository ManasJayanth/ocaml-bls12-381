let rustc_bls12_381 = import("@dannywillems/rustc-bls12-381").then((m) => {
    window._rustc_bls12_381 = m;
    let ocaml = import("./test.js").then((m) => {
        m.run();
    }).catch((e) => {
        console.log("Error while importing rustc-bls12-381 dep: " + e);
    });
});
