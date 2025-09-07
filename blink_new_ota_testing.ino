#include <WiFi.h>
#include <ArduinoOTA.h>

// --- Your Wi-Fi Credentials ---
const char *ssid = "Joydip";      // <-- IMPORTANT: Enter your Wi-Fi SSID
const char *password = "bhatpara";  // <-- IMPORTANT: Enter your Wi-Fi Password

// Define the pin for the built-in LED (GPIO2 is common for many ESP32 dev boards)
const int ledPin = 2;

void setup() {
  // Start serial communication for debugging
  Serial.begin(115200);
  Serial.println("\n\n--- Sketch Starting ---");

  // Initialize the LED pin
  pinMode(ledPin, OUTPUT);
  Serial.println("LED Pin initialized as OUTPUT.");

  // --- Standard Wi-Fi Connection Code ---
  Serial.print("Connecting to Wi-Fi network: ");
  Serial.println(ssid);
  WiFi.mode(WIFI_STA);
  WiFi.begin(ssid, password);
  while (WiFi.waitForConnectResult() != WL_CONNECTED) {
    Serial.println("Connection Failed! Rebooting...");
    delay(5000);
    ESP.restart();
  }
  Serial.println("\nWi-Fi Connected!");

  // --- Standard OTA Initialization Code ---
  Serial.println("Initializing OTA...");
  ArduinoOTA.onStart([]() { Serial.println("Start updating"); });
  ArduinoOTA.onEnd([]() { Serial.println("\nEnd"); });
  ArduinoOTA.onProgress([](unsigned int progress, unsigned int total) {
    Serial.printf("Progress: %u%%\r", (progress / (total / 100)));
  });
  ArduinoOTA.onError([](ota_error_t error) { Serial.printf("Error[%u]: ", error); });
  ArduinoOTA.begin();

  Serial.println("Ready for OTA updates and blinking!");
  Serial.print("IP address: ");
  Serial.println(WiFi.localIP());
  Serial.println("------------------------\n");
}

void loop() {
  // CRITICAL: Always call the OTA handle function to listen for updates
  ArduinoOTA.handle();

  // --- Blink Logic with Debugging ---
  digitalWrite(ledPin, HIGH);
  delay(1000);

  digitalWrite(ledPin, LOW);
  delay(1000);

  Serial.println("new code");
}