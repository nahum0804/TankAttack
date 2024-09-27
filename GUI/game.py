import pygame
import sys
import math
import time
import socket
import ast

pygame.init()

# Dimensiones de la ventana y la cuadrícula
SCREEN_WIDTH, SCREEN_HEIGHT = 600, 600
GRID_ROWS, GRID_COLS = 12, 12  # Filas y columnas del tablero
TILE_SIZE = SCREEN_WIDTH // GRID_COLS  # Tamaño de cada celda dinámico

# Colores
WHITE = (255, 255, 255)
BLUE = (0, 0, 255)
RED = (255, 0, 0)
BLACK = (0, 0, 0)
GRAY = (200, 200, 200)
GREEN = (0, 255, 0)

# Crear la ventana
screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
pygame.display.set_caption("Juego de Tablero")

board = [
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1],
    [1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1],
    [1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1],
    [1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1],
    [1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1],
    [1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1],
    [1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1],
    [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
]

player_pos = [1, 1]

player_lives = 3
last_shot_time = 0
enemy_last_shot_time = 0
shoot_interval = 500
bullet_speed = 0.1

enemy_up = pygame.transform.scale(pygame.image.load('./assets/sprite-enemy-up.png'), (TILE_SIZE, TILE_SIZE))
enemy_down = pygame.transform.scale(pygame.image.load('./assets/sprite-enemy-down.png'), (TILE_SIZE, TILE_SIZE))
enemy_left = pygame.transform.scale(pygame.image.load('./assets/sprite-enemy-left.png'), (TILE_SIZE, TILE_SIZE))
enemy_right = pygame.transform.scale(pygame.image.load('./assets/sprite-enemy-right.png'), (TILE_SIZE, TILE_SIZE))


def peticion(message):
    # Crear un socket TCP/IP
    client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Conectar con el servidor en la dirección IP y el puerto correctos
    server_address = ('127.0.0.1', 3000)
    client_socket.connect(server_address)
    response = None

    try:
        client_socket.sendall(message.encode())

        # Esperar y recibir respuesta del servidor
        data = client_socket.recv(1024)
        response = data.decode()
        response = ast.literal_eval(response)
    finally:
        # Cerrar el socket
        client_socket.close()
        return response

class Enemy():
    def __init__(self, position: list) -> None:
        self.position = position
        self.bullets = []
        self.current_image = enemy_up
        self.lives = 4
        self.movments = []
        self.is_exploding = False
        self.explosion_start_time = 0  

    def draw_enemy(self):
        if self.is_exploding:
            # Mostrar la imagen de explosión si el enemigo está explotando
            screen.blit(explosion_image, (self.position[1] * TILE_SIZE, self.position[0] * TILE_SIZE))
        else:
            # Mostrar la imagen normal del enemigo
            x = self.position[1] * TILE_SIZE
            y = self.position[0] * TILE_SIZE
            screen.blit(self.current_image, (x, y))

    def trigger_explosion(self):
        self.is_exploding = True
        self.explosion_start_time = pygame.time.get_ticks()  # Guardar el tiempo de inicio
        explosion_sound.play()

    def update_explosion(self):
        if self.is_exploding:
            current_time = pygame.time.get_ticks()
            if current_time - self.explosion_start_time > 1000:  # Si han pasado más de 1 segundo
                enemys.remove(self)  # Eliminar enemigo después de la explosión
                print("¡Enemigo eliminado!")

    def draw_enemy(self):
        if self.is_exploding:
            screen.blit(explosion_image, (self.position[1] * TILE_SIZE, self.position[0] * TILE_SIZE))
        else:
            x = self.position[1] * TILE_SIZE
            y = self.position[0] * TILE_SIZE
            screen.blit(self.current_image, (x, y))

    def draw_enemy_bullets(self):
        for bullet in self.bullets:
            x = bullet[0] * TILE_SIZE + TILE_SIZE // 2
            y = bullet[1] * TILE_SIZE + TILE_SIZE // 2
            pygame.draw.circle(screen, RED, (x, y), 5)

    def update_enemy_bullets(self):
        global player_lives
        enemy_bullets_to_remove = []
        for bullet in self.bullets[:]:
            bullet[0] += bullet[2] * bullet_speed  # Mover el disparo en el eje x
            bullet[1] += bullet[3] * bullet_speed  # Mover el disparo en el eje y

            if bullet[0] < 0 or bullet[0] >= GRID_COLS or bullet[1] < 0 or bullet[1] >= GRID_ROWS:
                enemy_bullets_to_remove.append(bullet)
                continue

            if board[int(bullet[1])][int(bullet[0])] == 1:
                print("¡La bala chocó contra un muro!")
                enemy_bullets_to_remove.append(bullet)
                continue

            if int(bullet[0]) == player_pos[1] and int(bullet[1]) == player_pos[0]:
                player_lives -= 1
                print("¡El enemigo te ha disparado!")
                enemy_bullets_to_remove.append(bullet)
                continue

            if player_lives <= 0:
                #pygame.time.delay(1000)
                #screen.blit(explosion_image, (player_pos[1] * TILE_SIZE, player_pos[0] * TILE_SIZE))
                #explosion_sound.play()
                show_game_over()
                pygame.quit()
                sys.exit()

        for bullet in enemy_bullets_to_remove:
            self.bullets.remove(bullet)

    def enemy_shoot(self):
        bullet_dir = [0, 0]
        distance_x = player_pos[1] - self.position[1]
        distance_y = player_pos[0] - self.position[0]

        if abs(distance_x) <= 3 and distance_y == 0:  # Solo horizontal

            if distance_x > 0:
                bullet_dir[0] = 1
                self.current_image = enemy_right
            else:
                bullet_dir[0] = -1
                self.current_image = enemy_left

        elif abs(distance_y) <= 3 and distance_x == 0:  # Solo vertical
            if distance_y > 0:
                bullet_dir[1] = 1
                self.current_image = enemy_down
            else:
                bullet_dir[1] = -1
                self.current_image = enemy_up

        if bullet_dir != [0, 0]:
            self.bullets.append([self.position[1], self.position[0], bullet_dir[0], bullet_dir[1]])
            bullet_sound.play()

    def move(self):
        if self.movments.__len__() != 0:
            newPos = self.movments[0]

            if newPos[0] < self.position[0]:
                self.current_image = enemy_up
            elif newPos[0] > self.position[0]:
                self.current_image = enemy_down
            elif newPos[1] < self.position[1]:
                self.current_image = enemy_left
            elif newPos[1] > self.position[1]:
                self.current_image = enemy_right

            if board[newPos[0]][newPos[1]] == 0:
                self.position[0] = newPos[0]
                self.position[1] = newPos[1]
                self.movments.remove(self.movments[0])
        else:
            message = f"{self.position[0]} {self.position[1]} {player_pos[0]} {player_pos[1]}"
            result = peticion(message)
            if result:
                self.movments = result[:5]

enemys = [Enemy([10, 10]), Enemy([10, 1]), Enemy([10, 5])]
bullets = []

def draw_board():
    for row in range(len(board)):
        for col in range(len(board[0])):
            tile = board[row][col]
            x = col * TILE_SIZE
            y = row * TILE_SIZE
            color = GRAY if tile == 1 else WHITE
            pygame.draw.rect(screen, color, (x, y, TILE_SIZE, TILE_SIZE))
            pygame.draw.rect(screen, BLACK, (x, y, TILE_SIZE, TILE_SIZE), 1)

direcction = 'u'

def obtain_sprite():
    if direcction == 'u':
        return pygame.transform.scale(pygame.image.load('./assets/sprite-tank-up.png'), (TILE_SIZE, TILE_SIZE))
    elif direcction == 'd':
        return pygame.transform.scale(pygame.image.load('./assets/sprite-tank-down.png'), (TILE_SIZE, TILE_SIZE))
    elif direcction == 'r':
        return pygame.transform.scale(pygame.image.load('./assets/sprite-tank-right.png'), (TILE_SIZE, TILE_SIZE))
    else:
        return pygame.transform.scale(pygame.image.load('./assets/sprite-tank-left.png'), (TILE_SIZE, TILE_SIZE))


def obtain_enemy_sprite(direction):
    if direction == 'u':
        return enemy_up
    elif direction == 'd':
        return enemy_down
    elif direction == 'r':
        return enemy_right
    else:
        return enemy_left

def draw_player():
    x = player_pos[1] * TILE_SIZE
    y = player_pos[0] * TILE_SIZE
    screen.blit(obtain_sprite(), (x, y))


def draw_bullets():
    for bullet in bullets:
        x = bullet[0] * TILE_SIZE + TILE_SIZE // 2
        y = bullet[1] * TILE_SIZE + TILE_SIZE // 2
        pygame.draw.circle(screen, GREEN, (x, y), 5)


def move_player(dx, dy):
    new_pos = [player_pos[0] + dy, player_pos[1] + dx]
    if board[new_pos[0]][new_pos[1]] == 0:  # Asegurarse de no colisionar con muros
        player_pos[0] += dy
        player_pos[1] += dx


def shoot():
    bullet_dir = [0, 0]

    if direcction == 'u':
        bullet_dir[1] = -1
    elif direcction == 'd':
        bullet_dir[1] = 1
    elif direcction == 'r':
        bullet_dir[0] = 1
    else:
        bullet_dir[0] = -1

    if bullet_dir != [0, 0]:
        bullet_x = int(player_pos[1])
        bullet_y = int(player_pos[0])
        bullets.append([bullet_x, bullet_y, bullet_dir[0], bullet_dir[1]])
        bullet_sound.play()


def update_bullets():
    global player_lives
    for enemy in enemys:
        bullets_to_remove = []
        for bullet in bullets:
            bullet[0] += bullet[2] * bullet_speed
            bullet[1] += bullet[3] * bullet_speed

            if bullet[0] < 0 or bullet[0] >= GRID_COLS or bullet[1] < 0 or bullet[1] >= GRID_ROWS:
                bullets_to_remove.append(bullet)
                continue

            if int(bullet[0]) == enemy.position[1] and int(bullet[1]) == enemy.position[0]:
                print("¡Disparo impactó al enemigo!")
                enemy.lives -= 1
                if enemy.lives == 0 and not enemy.is_exploding:
                    enemy.trigger_explosion()
                bullets_to_remove.append(bullet)
                continue

            if board[int(bullet[1])][int(bullet[0])] == 1:  # Si hay un muro
                print("¡La bala chocó contra un muro!")
                bullets_to_remove.append(bullet)

        for bullet in bullets_to_remove:
            bullets.remove(bullet)


def show_game_over():
    time.sleep(2)
    font = pygame.font.Font(None, 74)
    text = font.render("Game Over", True, BLACK)
    screen.blit(text, (SCREEN_WIDTH // 2 - text.get_width() // 2, SCREEN_HEIGHT // 2 - text.get_height() // 2))
    pygame.display.flip()
    time.sleep(3)

def show_victory():
    time.sleep(2)
    font = pygame.font.Font(None, 74)
    text = font.render("You Win!", True, BLACK)
    screen.blit(text, (SCREEN_WIDTH // 2 - text.get_width() // 2, SCREEN_HEIGHT // 2 - text.get_height() // 2))
    pygame.display.flip()
    time.sleep(3)

def draw_enemys():
    for enemy in enemys:
        enemy.draw_enemy()

def move_enemys():
    for enemy in enemys:
        enemy.move()


def draw_enemys_bullets():
    for enemy in enemys:
        enemy.draw_enemy_bullets()


def update_enemys_bullets():
    for enemy in enemys:
        enemy.update_enemy_bullets()


def enemys_shoot():
    for enemy in enemys:
        enemy.enemy_shoot()

def update_explosions():
    for enemy in enemys:
        enemy.update_explosion()

bullet_sound = pygame.mixer.Sound('./assets/piupiu.wav')
explosion_sound = pygame.mixer.Sound('./assets/explosion.wav')
explosion_image = pygame.transform.scale(pygame.image.load('./assets/explosion.png'), (TILE_SIZE, TILE_SIZE))


# Ciclo principal del juego
while True:
    screen.fill(WHITE)
    draw_board()
    draw_player()
    draw_enemys()
    draw_bullets()
    draw_enemys_bullets()

    # Mover las balas
    update_bullets()
    update_enemys_bullets()
    update_explosions()

    if not enemys:
        show_victory()
        pygame.quit()
        sys.exit()

    # Eventos del jugador
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            sys.exit()
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_LEFT:
                direcction = 'l'
                move_player(-1, 0)
            elif event.key == pygame.K_RIGHT:
                direcction = 'r'
                move_player(1, 0)
            elif event.key == pygame.K_UP:
                direcction = 'u'
                move_player(0, -1)
            elif event.key == pygame.K_DOWN:
                direcction = 'd'
                move_player(0, 1)
            elif event.key == pygame.K_SPACE:
                current_time = pygame.time.get_ticks()
                if current_time - last_shot_time > shoot_interval:
                    shoot()
                    last_shot_time = current_time

    current_time = pygame.time.get_ticks()
    if current_time - enemy_last_shot_time > shoot_interval:
        enemys_shoot()
        enemy_last_shot_time = current_time
        move_enemys()
    pygame.display.flip()
    time.sleep(0.05)