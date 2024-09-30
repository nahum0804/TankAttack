# TankAttack

## Tabla de Contenidos

1. [Descripción](#descripción)
2. [Características](#características)
3. [Prerrequisitos](#prerrequisitos)
4. [Instalación](#instalación)
5. [Instrucciones](#instrucciones)

## Descripción

Este proyecto consiste en un juego de tanques en el cual el objetivo es eliminar a todos los tanques enemigos antes de perder las 3 vidas que tenemos. La interfaz del juego está desarrollada en Python utilizando la librería de Pygame, y el backend está desarrollado en Haskell usando Cabal.

## Características

- El tanque del jugador es de color verde, aparece en la posición [1,1] y tiene 3 vidas.
- Los tanques enemigos son de color rojo, aparecen aleatoriamente en el mapa y tienen 4 vidas.

### Prerrequisitos

- Algún editor de código que soporte varios lenguajes (Este proyecto se desarrolló con VS Code).
- Tener Python y Haskell instalados en el equipo.

## Instalación

- Para este proyecto es importante que se instale Cabal en el equipo:
  
  Comando Windows: `sudo apt-get install ghc cabal-install`

### Instrucciones

1. Poner a correr el servidor de Haskell:

    - Entrar a la carpeta del proyecto, ir a la terminal y navegar a la carpeta de `BackEnd` con `cd BackEnd`.
    - Aplicar los siguientes comandos:
      ```
      cabal update
      cabal install --only-dependencies
      cabal build
      cabal run
      ```
    - Ya con eso debería decir que el servidor se está corriendo. Si no, un posible error es:

2. Abrir el juego con Python:

    - Para ejecutar el juego con Python se debe abrir una nueva ventana en el editor de código, específicamente en la carpeta de `GUI` para evitar errores con la importación de los assets al proyecto.
    - Luego poner a correr el documento `game.py` para jugar.
    - Si genera algún error, cerciorarse de que el servidor esté corriendo.
