import pygame
import sys

# Inicializar Pygame
pygame.init()

# Tamaño de la pantalla
screen_width = 640
screen_height = 480
screen = pygame.display.set_mode((screen_width, screen_height))
pygame.display.set_caption("Jugador con Sprites")

# Colores
WHITE = (255, 255, 255)

# Cargar las imágenes del jugador
player_up = pygame.image.load('./assets/sprite-tank-up.png')
player_down = pygame.image.load('./assets/sprite-tank-down.png')
player_left = pygame.image.load('./assets/sprite-tank-left.png')
player_right = pygame.image.load('./assets/sprite-tank-right.png')

# Definir posición inicial del jugador
player_x = screen_width // 2
player_y = screen_height // 2
player_speed = 5

# Imagen actual del jugador (inicialmente hacia abajo)
current_image = player_down

# Bucle principal del juego
running = True
while running:
    # Controlar eventos
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # Obtener las teclas presionadas
    keys = pygame.key.get_pressed()

    # Actualizar posición del jugador y cambiar el sprite
    if keys[pygame.K_UP]:
        player_y -= player_speed
        current_image = player_up
    elif keys[pygame.K_DOWN]:
        player_y += player_speed
        current_image = player_down
    elif keys[pygame.K_LEFT]:
        player_x -= player_speed
        current_image = player_left
    elif keys[pygame.K_RIGHT]:
        player_x += player_speed
        current_image = player_right

    # Limpiar la pantalla
    screen.fill(WHITE)

    # Dibujar la imagen actual del jugador en la nueva posición
    screen.blit(current_image, (player_x, player_y))

    # Actualizar la pantalla
    pygame.display.flip()

    # Controlar la velocidad del juego
    pygame.time.Clock().tick(30)

# Salir del juego
pygame.quit()
sys.exit()
