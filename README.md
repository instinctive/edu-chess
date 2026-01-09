# edu-chess

Chess bot WIP.

Current status: rules of chess implemented,
can play a game with random or human players.

```bash
$ nix-shell
$ cabal run -v0 chess -- human random
```

![Screenshot of the chess program](/assets/chess.png)

## TODO

- Zobrist hashing does not include castling rights,
  en passant square, nor whose turn it is.
- Make an AI player =D
