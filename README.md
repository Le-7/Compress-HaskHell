# Compress

Bienvenue dans **compress**, un programme de compression de données en Haskell !

## Description

Ce programme propose plusieurs méthodes de compression pour réduire la taille des données. Actuellement, il prend en charge les méthodes suivantes :

- **RLE** (Run-Length Encoding)
- **LZ78** (Lempel-Ziv 78)
- **LZW** (Lempel-Ziv-Welch)
- **Huffman**
- **Shannon-Fano**

Vous pouvez choisir la méthode de compression souhaitée à partir d'un menu interactif et compresser vos données en conséquence.

## Installation

1. Assurez-vous d'avoir Haskell installé sur votre système.
2. Clonez ce dépôt :
    ```
    git clone https://github.com/votre-utilisateur/compress.git
    ```
3. Accédez au répertoire du projet :
    ```
    cd compress
    ```
4. Utilisez Stack pour installer les dépendances et exécuter le programme :
    ```
    stack upgrade (pour être sûr d'avoir la derniere version)
    stack build
    stack run compress-exe
    ```

## Utilisation

- Exécutez le programme à l'aide de la commande ci-dessus.
- Choisissez la méthode de compression souhaitée dans le menu interactif.
- Suivez les instructions pour entrer les données à compresser.
- Le programme affichera les données compressées ainsi que les données originales (décompressées) si possible.

## Tests

Les tests unitaires sont inclus dans le fichier `Spec.hs`. Pour les exécuter, utilisez la commande suivante :

```
stack test
```
