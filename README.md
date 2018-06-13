# hagit

A basic version control system written in Haskell (learning project) inspiried by the Git protocol.

## Installation

In order to build `hagit`, [`stack`](http://docs.haskellstack.org/en/stable/README.html) must be installed.  Once that is done, clone and build:
```bash
$ stack setup
$ stack build
```
After building, use `stack exec hagit` to run, or copy the generated binary to another directory.

## Usage

Assuming that the `hagit` binary is `./hagit`:
```bash
$ ./hagit <command> [arguments]
```

If developing, use:
```bash
$ stack exec hagit <command> [arguments]
```

Valid commands are:

- `init`: initializes the project directory, which is needed to be able to run other `hagit` commands on it.
- `commit`: stores all of the project's files in a commit, identified by a unique hash and a message, which must be passed as an additional argument.
- `checkout`: restores the project directory to the state it was on a given commit.  A valid commit hash must be specified as an additional argument.
- `log`: lists all commits in chronological order, showing hash, date and message for each one of them.
- `add`: adds specified files or directory to staging area
- `remove`: removes specified files or directory from staging area
- `status`: prints the current commit hash, and lists any new, deleted or modified in working tree, staging area and last commit.
- `help`: displays help on how to operate `hagit`.

## Used libraries

- `cryptohash`: SHA1 hashing for text objects
- `bytestring`: handling byte strings - needed in compression
- `directory`, `filepath`: operations on file system and it's file tree
- `containers`: Map type for displaying changes in `Status` command
- `time`: handling commit's date and time
- `conduit`: parallel file processing
- `strict`: strict readFile operation
- `Diff`: as a base for diff and mergeoperations
