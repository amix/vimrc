# Vader Tests

> Requires Vim 8 or Neovim, due to the way dependencies are installed.

To run the tests, you need to install the dependencies first. Use the
[installation script](install.sh):

```sh
test/install.sh
```

## Running the tests from the command line

You can run the tests with the following command:

```sh
vim -u test/vimrc -c 'Vader! test/*.vader'
```

## Running the tests from within Vim

Open vim with:

```
vim -u test/vimrc
```

Then, you can run the tests with the following command:

```vim
:Vader test/*.vader
```
