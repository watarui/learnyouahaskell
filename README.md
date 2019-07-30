# learnyouahaskell

本を読んで勉強

### テキスト

- [すごい Haskell たのしく学ぼう！ \| Ohmsha](https://www.ohmsha.co.jp/book/9784274068850/)
- [Learn You a Haskell for Great Good\!](http://learnyouahaskell.com/)

### 環境構築

#### stack

```sh
brew install stack

stack --version
Version 2.1.3, Git revision 0fa51b9925decd937e4a993ad90cb686f88fa282 (7739 commits) x86_64 hpack-0.31.2

stack setup
stack new learnyouahaskell

echo 'alias ghci="stack exec ghci"' >> ~/.zshrc
```

#### HIE

```sh
git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
cd haskell-ide-engine

./install.hs help
./install.hs hie-8.6.5
./install.hs build-data

echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc


hie --version
Version 0.11.0.0, Git revision e6247193dc4e3cb089de7025e956a72496cde1b6 (2933 commits) x86_64 ghc-8.6.5
```

#### VSCode

```sh
code --install-extension alanz.vscode-hie-server
```

### REPL

```sh
stack exec ghci
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Prelude> :q
Leaving GHCi.
```

### Build

```sh
stack build
```

### 参考

- [Haskell Language](https://www.haskell.org/)
- [Home \- The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- [Haskell Language Server \- Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server)
- [haskell/haskell\-ide\-engine: The engine for haskell ide\-integration\. Not an IDE](https://github.com/haskell/haskell-ide-engine#building)
