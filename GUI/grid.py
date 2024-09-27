import socket
import ast


def peticion(mensaje):
    # Crear un socket TCP/IP
    client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Conectar con el servidor en la dirección IP y el puerto correctos
    server_address = ('127.0.0.1', 3000)
    print(f'Conectando a {server_address[0]} en el puerto {server_address[1]}')
    client_socket.connect(server_address)

    try:
        print(f'Enviando: {mensaje}')
        client_socket.sendall(mensaje.encode())

        # Esperar y recibir respuesta del servidor
        data = client_socket.recv(1024)
        response = data.decode()
        response = ast.literal_eval(response)
        print(len(response))
        print(type(response))
        print(f'Recibido: {response}')

    finally:
        # Cerrar el socket
        print('Cerrando conexión')
        client_socket.close()

peticion("0 0 2 3")
peticion("0 0 2 3")
peticion("0 0 2 3")