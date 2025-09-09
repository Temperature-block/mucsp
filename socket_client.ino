#include <WiFi.h>
#include <sys/socket.h>

const char* ssid = "Joydip";
const char* password = "bhatpara";

const char* serverIP = "192.168.29.182";
uint16_t serverPort = 2025;


void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("\nConnected to WiFi");
  Serial.print("ESP32 IP Address: ");
  Serial.println(WiFi.localIP());
} // NOTE: An extra '}' was removed from here.

void loop() {
  int clientSocket = socket(AF_INET, SOCK_STREAM, 0);
  if (clientSocket < 0) {
    Serial.println("failed to create socket");
    delay(1000);
    return;
  }

  // FIXED: struct name is 'sockaddr_in' and variable name was misspelled
  struct sockaddr_in serverAddr;
  serverAddr.sin_family = AF_INET;
  serverAddr.sin_addr.s_addr = inet_addr(serverIP);
  serverAddr.sin_port = htons(serverPort);

  if (connect(clientSocket, (struct sockaddr *)&serverAddr, sizeof(serverAddr)) != 0) {
    Serial.println("Failed to connect to server");
    close(clientSocket);
    delay(1000);
    return;
  } else {
    // FIXED: Typo in 'Serial.println' and added missing semicolon
    Serial.println("connection successful");
  }

  // FIXED: '0' was a character, now it's the integer 0
  int device_ID = 0;
  // FIXED: 'string' (lowercase) changed to 'String' (uppercase)
  String message = String(device_ID) + ";" + WiFi.localIP().toString();

  send(clientSocket, message.c_str(), message.length(), 0);
  Serial.println("Message sent: " + message);
  
  close(clientSocket);
  Serial.println("Socket closed");
  
  delay(10000);
}
