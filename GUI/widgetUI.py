import tkinter as tk
import random

class TankAttackVentana:
    def __init__(self, root):
        self.root = root
        self.root.title("Tank Attack")
        self.root.geometry("800x600")

        self.board_size = 20  # 20x20 grid
        self.cell_size = 30   # Size of each cell in the grid
        self.enemies = 5      # Number of enemy tanks
        self.movement_delay = 200  # Delay in milliseconds between movements

        self.canvas = None  # Placeholder for the game canvas
        self.player_position = [self.board_size // 2, self.board_size // 2]  # Player starts in the center
        self.moving = False  # To track if the player is currently moving
        self.bullet_active = False  # Track if there's an active bullet
        self.bullet_direction = None  # Track the direction of the bullet

        self.label = tk.Label(self.root, text="Tank Attack", font=("Helvetica", 24, "bold"), fg="white", bg="black")
        self.label.pack(expand=True)
        self.root.after(4000, self.mostrar_nivel)

        self.root.bind("<KeyPress>", self.key_press)

    def mostrar_nivel(self):
        self.label.config(text="Level 1")
        self.root.after(4000, self.mostrar_juego)

    def mostrar_juego(self):
        self.label.config(text="¡Comienza el juego!")
        self.root.after(4000, self.mostrar_pantalla_juego)

    def mostrar_pantalla_juego(self):
        self.label.destroy()
        self.canvas = tk.Canvas(self.root, width=self.board_size * self.cell_size, height=self.board_size * self.cell_size, bg="black")
        self.canvas.pack()
        self.create_board()

    def create_board(self):
        self.grid = []

        for i in range(self.board_size):
            row = []
            for j in range(self.board_size):
                # Add walls randomly in the grid (10% chance for a wall)
                if random.random() < 0.1 and (i != self.board_size // 2 or j != self.board_size // 2):
                    wall = self.canvas.create_rectangle(
                        j * self.cell_size, i * self.cell_size,
                        (j + 1) * self.cell_size, (i + 1) * self.cell_size,
                        fill="gray", outline="black")
                    row.append("W")  # Wall
                else:
                    cell = self.canvas.create_rectangle(
                        j * self.cell_size, i * self.cell_size,
                        (j + 1) * self.cell_size, (i + 1) * self.cell_size,
                        fill="green", outline="black")
                    row.append("E")  # Empty space
            self.grid.append(row)

        # Place player's tank at the center
        self.player_tank = self.canvas.create_rectangle(
            self.player_position[1] * self.cell_size, self.player_position[0] * self.cell_size,
            (self.player_position[1] + 1) * self.cell_size, (self.player_position[0] + 1) * self.cell_size,
            fill="blue", outline="black")
        self.grid[self.player_position[0]][self.player_position[1]] = "P"  # P for Player

        # Place enemy tanks randomly
        for _ in range(self.enemies):
            while True:
                x, y = random.randint(0, self.board_size - 1), random.randint(0, self.board_size - 1)
                if self.grid[x][y] == "E":  # Empty space
                    enemy_tank = self.canvas.create_rectangle(
                        y * self.cell_size, x * self.cell_size,
                        (y + 1) * self.cell_size, (x + 1) * self.cell_size,
                        fill="red", outline="black")
                    self.grid[x][y] = "T"  # T for Tank
                    break

    def move_player(self, dx, dy):
        new_x = self.player_position[0] + dx
        new_y = self.player_position[1] + dy

        # Ensure the player stays within bounds and doesn't move into walls
        if 0 <= new_x < self.board_size and 0 <= new_y < self.board_size and self.grid[new_x][new_y] == "E":
            # Update grid
            self.grid[self.player_position[0]][self.player_position[1]] = "E"  # Mark old position as empty
            self.player_position = [new_x, new_y]
            self.grid[new_x][new_y] = "P"  # Mark new position as player

            # Update player's tank position on canvas
            self.canvas.coords(self.player_tank,
                               new_y * self.cell_size, new_x * self.cell_size,
                               (new_y + 1) * self.cell_size, (new_x + 1) * self.cell_size)

        # Allow movement again after the delay
        self.root.after(self.movement_delay, self.enable_movement)

    def enable_movement(self):
        self.moving = False

    def fire_bullet(self, direction):
        if self.bullet_active:
            return  # Only one bullet at a time

        self.bullet_direction = direction
        bullet_x, bullet_y = self.player_position
        self.bullet_active = True
        self.bullet_id = self.canvas.create_rectangle(
            bullet_y * self.cell_size + 10, bullet_x * self.cell_size + 10,
            (bullet_y + 1) * self.cell_size - 10, (bullet_x + 1) * self.cell_size - 10,
            fill="yellow", outline="red")

        self.move_bullet(bullet_x, bullet_y, 0)

    def move_bullet(self, x, y, distance_traveled):
        if distance_traveled > 7 or not (0 <= x < self.board_size) or not (0 <= y < self.board_size) or self.grid[x][y] == "W":
            # Remove bullet if it travels 7 cells, hits a wall, or goes out of bounds
            self.canvas.delete(self.bullet_id)
            self.bullet_active = False
            return

        # Move the bullet in the current direction
        if self.bullet_direction == 'up':
            x -= 1
        elif self.bullet_direction == 'down':
            x += 1
        elif self.bullet_direction == 'left':
            y -= 1
        elif self.bullet_direction == 'right':
            y += 1

        # Update the position of the bullet on the canvas
        self.canvas.coords(self.bullet_id,
                           y * self.cell_size + 10, x * self.cell_size + 10,
                           (y + 1) * self.cell_size - 10, (x + 1) * self.cell_size - 10)

        # Continue moving the bullet
        self.root.after(100, self.move_bullet, x, y, distance_traveled + 1)

    def key_press(self, event):
        # Default direction for now (can be improved)
        direction = self.bullet_direction
        if not self.moving:
            if event.keysym == 'w':  # Move up
                self.moving = True
                self.move_player(-1, 0)
            elif event.keysym == 's':  # Move down
                self.moving = True
                self.move_player(1, 0)
            elif event.keysym == 'a':  # Move left
                #Change fire_bullet here
                self.moving = True
                self.move_player(0, -1)
            elif event.keysym == 'd':  # Move right
                self.moving = True
                self.move_player(0, 1)
            elif event.keysym == 'space':  # Fire bullet
                if direction == None:
                    self.fire_bullet('up')  # Default direction for now (can be improved)
                self.fire_bullet(direction) # Verificar aquí la movilidad del disparo

if __name__ == "__main__":
    root = tk.Tk()
    app = TankAttackVentana(root)
    root.mainloop()