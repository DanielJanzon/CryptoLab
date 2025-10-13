# Next Step Toward a Usable CryptoLab Chat

The repository already exposes the primitives needed to stand up an encrypted chat:

* `ElGamalMain` lets us generate a safe prime group, derive key pairs, and perform the raw encrypt/decrypt operations from the command line.【F:ElGamalMain.hs†L1-L60】
* `Encoder` translates byte streams to group elements and back so that chat messages can be transported as integers below the modulus.【F:Encoder.hs†L1-L64】
* `Random` provides cryptographically strong randomness by sampling `/dev/urandom`, covering both n-bit integers for prime search and bounded integers for ephemeral keys.【F:Random.hs†L1-L20】

What is missing is the *session orchestration* described in the README: there is no code that actually opens a TCP connection, negotiates key material, maintains the one-time pad buffer, or plugs these pieces into a chat loop.【F:README†L14-L37】 That functionality is the critical next step to transform the existing building blocks into a functioning application.

### Recommended next milestone
Implement a networking layer (e.g., a `Chat.hs` module) that:

1. Establishes a TCP connection between peers.
2. Uses `ElGamalMain` primitives to exchange fresh randomness securely until both sides have a sufficient buffer for the one-time pad.
3. Encodes outgoing messages with `Encoder.encode`, XORs them with buffered randomness, and sends them over the socket.
4. Decodes incoming ciphertext by reversing the process and delivering plaintext to the user interface.

Delivering this orchestration module—together with a simple CLI wrapper to drive it—will bring the project from a cryptographic toolkit to the interactive encrypted chat envisioned by the README.
