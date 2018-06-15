# hagit

A basic version control system written in Haskell based on the Git protocol.

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

During development use:
```bash
$ stack exec hagit <command> [arguments]
```

Valid commands are:

- `init`: initializes the project directory required to run other `hagit` commands on it.
- `commit`: stores all of the project's files in a commit, identified by a unique hash and a message.
- `checkout`: restores the project directory to the state of the given commit or branch. If supplied with nonexistent branch name - creates new branch. Potentially merges files content.
- `log`: lists all commits in chronological order with hash, parent's hash, date and message.
- `add`: adds specified files or directory to staging area.
- `remove`: removes specified files or directory from staging area.
- `branch`: lists branches.
- `diff`: diffs files with versions in staging area; output like UNIX diff.
- `status`: prints the current commit hash, and lists any new, deleted or modified in working tree, staging area and last commit.
- `help`: displays help.

## Used libraries

- `cryptohash`: SHA1 hashing for text objects
- `bytestring`: handling byte strings - needed in compression
- `directory`, `filepath`: operations on file system and it's file tree
- `containers`: Map type for displaying changes in `Status` command
- `time`: handling commit's date and time
- `conduit`: parallel file processing
- `strict`: strict readFile operation
- `Diff`: as a base for diff and merge operations

## Attribution

Initially based on hvc-master (https://github.com/federicotdn/hvc) by Federico T. 

Diff code adapted from Data.Algorithm.DiffOutput module ((c) Sterling Clover 2008-2011, Kevin Charter 2011 - available at https://hub.darcs.net/sterlingclover/Diff/browse/src/Data/Algorithm/DiffOutput.hs)