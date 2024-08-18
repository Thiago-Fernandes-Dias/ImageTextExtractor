# Image Text Extractor

Serviço para extração do texto em imagens feito em Haskell.

## Dependências

Para que o projeto seja executado localmente (em um ambiente Linux), certifique-se de que as dependências

- zlib1g-dev
- build-essential
- libffi-dev
- libgmp-dev
- libgmp10
- libncurses-dev
- tesseract-ocr
- ghc `v9.4.8`
- cabal `v3.10.3.0`

estejam instaladas no sistema.

## Instalando e executando localmente

Para rodar o **ImageTextExtractor** localmente,

- atualize o repositório de dependências

```bash
cabal update
```

- e execute o projeto com

```bash
cabal run
```
