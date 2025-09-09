import socket

server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

HOST = '0.0.0.0'
PORT=2025

server_socket.bind((HOST, PORT))
server_socket.listen()
print("the host and port are", HOST, PORT)

while True:
    client_socket,addr=server_socket.accept()
    print(f"connected from {addr}")
    ID=client_socket.recv(64) #id has to be sent in a id;ip format
    print(f"recived ID is {ID}")
    decoded_ID=ID.decode("utf-8")
    print(f"ID of the device is {decoded_ID}")
    
    try:
        id_str,ip_str=decoded_ID.split(";")
        device_ID=int(id_str)
        print(f"device ID {device_ID} and ip is {ip_str}")
        
    except ValueError:
        print("ip and id sent with incorrect format")
        
