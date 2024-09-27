def cell_from_int(val):
    return 'Wall' if val == 1 else 'Empty'

# Función para generar la matriz con coordenadas
def generate_matrix(board, start_x, start_y, step):
    matrix_str = "matrizEjemplo = [\n"
    for y, row in enumerate(board):
        matrix_str += "    ["
        for x, val in enumerate(row):
            coord_x = start_x + (x * step)
            coord_y = start_y + (y * step)
            cell_type = cell_from_int(val)
            matrix_str += f"Coordinate ({coord_x}, {coord_y}) {cell_type}, "
        matrix_str = matrix_str.rstrip(", ")  # Remove trailing comma
        matrix_str += "],\n"
    matrix_str += "]"
    return matrix_str

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

start_x = -200
start_y = -200
step = 50

matrix_output = generate_matrix(board, start_x, start_y, step)
print(matrix_output)