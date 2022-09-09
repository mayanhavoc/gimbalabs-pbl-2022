# Game Instruction

## Zero One Game Logic

This game implement to be played between 2 players. It is a bit like rock paper scissors, but even simpler as there are only two options. First player and second player both have two options, they can either play 0 or 1. Depending on what they play, one of them wins.

- If they both choose same option, either both choose 0 or both choose 1, then **first player** wins.

- If the choices are different, then **second player** wins.

First player can send his choice with **Datum**, but string inside **Datum** in their default format is just plain text converted to `ByteString`. So this of course gives a very unfair advantage to second player. Second player now can check first player's submitted **Datum** and sees what number first player was picked.

We'll use a cryptographic protocol called commit schemes. The idea is that first player does not reveal his choice to second player, but he commits to his choice so that he later can not change his mind.

We'll use hash functions and they are one way functions. So It is difficult given a hash to reconstruct the original document or the original byte string that was hashed to this hash.

Looking at the game, first player, instead of putting his choice in plain text inside **Datum**, he instead puts the hash of his choice while it was concatenate with some arbitrary text called **nonce**.

Why you may ask, because hash of 0 or 1 will be always same which may look very cryptic to second player the first time they play. However, sooner or later he will notice that he always sees one of these two hashes and then he knows which choice first player made.

So let's say first player puts the hash of his 0 concatenated with **nonce** at **Datum**, which is just a cryptic byte string. So now second player sees this cryptic byte string and he has no idea whether first player picked, 0 or 1. But for second player there would be no need for him to use a hash, so he can just put the choice in plain text inside **Datum**. And second player choice 0 as well.

In this case first player would have won, however second player still has no proof that first player has won. So there is then one additional step, that first player has to show his actual choice with **nonce**, which he'll use **Redeemer** for that.

## How to Play

After obtaining and specifying the following, compile the game contract and get its address in order to start playing.

- Payment public key hash of first player and second player.
- Stake public key hash of first player and second player.
- Specify game bet amount.
- Specify time for **game deadline** and **reveal deadline**.
- Obtain currency symbol and token name of state NFT.
- Provide **nonce** and choose 0 or 1 at `dataToJSON.hs`.
- Change second player choice at `dataToJSON.hs` file from `Zero` to `One` or keep it unchanged.
- Run `cabal run dataToJSON` to create redeemer and datum files.

Now that you have all the parts and necessary files (redeemers and datums) you can transfer all files inside `output` folder to destination place you want to run `cardano-cl` commands or you can start the game at `output` folder.

You are ready to play so choose to play as either both players (needs 2 wallet) or one player. In either case follow below process to have a unique on-chain fun :) .

1. First, first player start the game by sending **bet amount** plus **state NFT** to game smart contract along with the hash of his nonce combined with the choice inside `startGame-DATUM.json` datum.

2. Then if second player start playing before **game deadline**, he can take game UTxO inside game smart contract (which was sent by first player) and consume it and produce new UTxO with his own choice and send it back to game smart contract, by using these files `secondPlayerPlayChoice-DATUM.json` datum and `secondPlayerPlayChoice-REDEEMER.json` redeemer.

3. If at this point, first player realizes that he has won depending on second player's choice, he will consume game UTxO and reveal his secret (nonce) which will be inside `firstPlayerRevealChoice-REDEEMER.json` and the game ends with his winning.

4. If however, after second player makes his move, first player sees that he has lost, there's no need actually for him to do anything. So after **Reveal Deadline** has been, second player can claims his win by using `secondPlayerClaims-REDEEMER.json`. By the way, second player has to send back state NFT to first player and he has to provide **Shelly Era Address** of first player.

5. One last thing, that after first player starts playing, second player simply is not interested and doesn't play. So, in that case, first player can get his own money back after **game deadline** has been reached, by using `firstPlayerClaims-REDEEMER.json`.

## First Player Start The Game

```bash
MAGIC=--testnet-magic 1
TXIN=
NFT_TXIN=
CONTRACT_ADDR=ZeroOneGame.addr
FIRST_PLAYER_BET_AMOUNT=3000000
NFT_ADA_AMOUNT=2000000
AMOUNT=$(expr $FIRST_PLAYER_BET_AMOUNT + $NFT_ADA_AMOUNT)
START_DATUM=startGame-DATUM.json
STATE_NFT=
FIRST_PLAYER=
```
  
```bash
cardano-cli transaction build \
--alonzo-era \
$MAGIC \
--tx-in $TXIN \
--tx-in $NFT_TXIN \
--tx-out "$(cat $CONTRACT_ADDR) $AMOUNT lovelace + 1 $STATE_NFT" \
--tx-out-datum-hash-file $START_DATUM \
--change-address $(cat $FIRST_PLAYER.addr) \
--out-file tx.body

cardano-cli transaction sign \
$MAGIC \
--tx-body-file tx.body \
--signing-key-file $FIRST_PLAYER.skey \
--out-file tx.signed

cardano-cli transaction submit $MAGIC --tx-file tx.signed
```

## Second Player Start To Play

```bash
MAGIC=--testnet-magic 1
TXIN=
CONTRACT_TXIN=
CONTRACT_FILE=ZeroOneGame.plutus
START_DATUM=startGame-DATUM.json
PLAY_REDEEMER=secondPlayerPlayChoice-REDEEMER.json
COLLATERAL=
PLAY_DATUM=secondPlayerPlayChoice-DATUM.json
CONTRACT_ADDR=ZeroOneGame.addr
GAME_UTXO_AMOUNT=5000000
SECOND_PLAYER_BET_AMOUNT=3000000
AMOUNT=$(expr $GAME_UTXO_AMOUNT + $SECOND_PLAYER_BET_AMOUNT)
STATE_NFT=
SECOND_PLAYER=
NOW_SLOT_NUM=
GAME_DEADLINE_SLOT_NUM=$(expr $NOW_SLOT_NUM + 3600)
```

```bash
cardano-cli transaction build \
--alonzo-era \
$MAGIC \
--tx-in $TXIN \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $CONTRACT_FILE \
--tx-in-datum-file $START_DATUM \
--tx-in-redeemer-file $PLAY_REDEEMER \
--tx-in-collateral $COLLATERAL \
--tx-out-datum-embed-file $PLAY_DATUM \
--tx-out "$(cat $CONTRACT_ADDR) $AMOUNT lovelace + 1 $STATE_NFT" \
--change-address $(cat $SECOND_PLAYER.addr) \
--required-signer $SECOND_PLAYER.skey \
--invalid-hereafter $GAME_DEADLINE_SLOT_NUM \
--protocol-params-file protocol.json \
--out-file tx.body

cardano-cli transaction sign \
$MAGIC \
--tx-body-file tx.body \
--signing-key-file $SECOND_PLAYER.skey \ 
--out-file tx.signed

cardano-cli transaction submit $MAGIC --tx-file tx.signed
```

## First Player Reveal The Choice

```bash
MAGIC=--testnet-magic 1
TXIN=
CONTRACT_TXIN=
CONTRACT_FILE=ZeroOneGame.plutus
PLAY_DATUM=secondPlayerPlayChoice-DATUM.json
REVEAL_REDEEMER=firstPlayerRevealChoice-REDEEMER.json
COLLATERAL=
NFT_ADA_AMOUNT=2000000
WIN_AMOUNT=6000000
AMOUNT=$(expr $NFT_ADA_AMOUNT + $WIN_AMOUNT)
STATE_NFT=
FIRST_PLAYER=
NOW_SLOT_NUM=
REVEAL_DEADLINE_SLOT_NUM=$(expr $NOW_SLOT_NUM + 1800)
```

```bash
cardano-cli transaction build \
--alonzo-era \
$MAGIC \
--tx-in $TXIN \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $CONTRACT_FILE \
--tx-in-datum-file $PLAY_DATUM \
--tx-in-redeemer-file $REVEAL_REDEEMER \
--tx-in-collateral $COLLATERAL \
--tx-out "$(cat $FIRST_PLAYER.addr) $AMOUNT lovelace + 1 $STATE_NFT" \
--change-address $(cat $FIRST_PLAYER.addr) \
--required-signer $FIRST_PLAYER.skey \
--invalid-hereafter $REVEAL_DEADLINE_SLOT_NUM \
--protocol-params-file protocol.json \
--out-file tx.body

cardano-cli transaction sign \
$MAGIC \
--tx-body-file tx.body \
--signing-key-file $FIRST_PLAYER.skey \
--out-file tx.signed

cardano-cli transaction submit $MAGIC --tx-file tx.signed
```

## First Player Claims Its Own Amount

```bash
MAGIC=--testnet-magic 1
TXIN=
CONTRACT_TXIN=
CONTRACT_FILE=ZeroOneGame.plutus
START_DATUM=startGame-DATUM.json
FIRST_CLAIMS_REDEEMER=firstPlayerClaims-REDEEMER.json
COLLATERAL=
FIRST_PLAYER_BET_AMOUNT=3000000
NFT_ADA_AMOUNT=2000000
AMOUNT=$(expr $FIRST_PLAYER_BET_AMOUNT + $NFT_ADA_AMOUNT)
STATE_NFT=
FIRST_PLAYER=
NOW_SLOT_NUM=
```

```bash
cardano-cli transaction build \
--alonzo-era \
$MAGIC \
--tx-in $TXIN \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $CONTRACT_FILE \
--tx-in-datum-file $START_DATUM \
--tx-in-redeemer-file $FIRST_CLAIMS_REDEEMER \
--tx-in-collateral $COLLATERAL \
--tx-out "$(cat $FIRST_PLAYER.addr) $AMOUNT lovelace + 1 $STATE_NFT" \
--change-address $(cat $FIRST_PLAYER.addr) \
--required-signer $FIRST_PLAYER.skey \
--invalid-before $NOW_SLOT_NUM \
--protocol-params-file protocol.json \
--out-file tx.body

cardano-cli transaction sign \
$MAGIC \
--tx-body-file tx.body \
--signing-key-file $FIRST_PLAYER.skey \
--out-file tx.signed

cardano-cli transaction submit $MAGIC --tx-file tx.signed
```

## Second Player Claims Wining Amount

```bash
MAGIC=--testnet-magic 1
TXIN=
CONTRACT_TXIN=
CONTRACT_FILE=ZeroOneGame.plutus
PLAY_DATUM=secondPlayerPlayChoice-DATUM.json
SECOND_CLAIMS_REDEEMER=secondPlayerClaims-REDEEMER.json
COLLATERAL=
WIN_AMOUNT=6000000
NFT_ADA_AMOUNT=2000000
STATE_NFT=
SECOND_PLAYER=
FIRST_PLAYER=
NOW_SLOT_NUM=
```

```bash
cardano-cli transaction build \
--alonzo-era \
$MAGIC \
--tx-in $TXIN \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $CONTRACT_FILE \
--tx-in-datum-file $PLAY_DATUM \
--tx-in-redeemer-file $SECOND_CLAIMS_REDEEMER \
--tx-in-collateral $COLLATERAL \
--tx-out "$(cat $SECOND_PLAYER.addr) $(expr $WIN_AMOUNT - 2000000) lovelace" \
--tx-out "$(cat $FIRST_PLAYER.saddr) $NFT_ADA_AMOUNT lovelace + 1 $STATE_NFT" \
--change-address $(cat $SECOND_PLAYER.addr) \
--required-signer $SECOND_PLAYER.skey \
--invalid-before $NOW_SLOT_NUM \
--protocol-params-file protocol.json \
--out-file tx.body

cardano-cli transaction sign \
$MAGIC \
--tx-body-file tx.body \
--signing-key-file $SECOND_PLAYER.skey \
--out-file tx.signed

cardano-cli transaction submit $MAGIC --tx-file tx.signed
```
