#python compile_and_upload.py --sketch-dir "D:\code stuff\new-occam\blink_new_ota_testing" --fqbn esp32:esp32:esp32doit-devkit-v1 --ip 192.168.29.189
import subprocess
import argparse
import sys
import os
import tempfile
import shutil

def find_tool(tool_name):
    """Checks if a tool exists in the system's PATH."""
    return shutil.which(tool_name)

def find_espota():
    """Tries to find the espota.py script in common Arduino IDE installation locations."""
    # This logic is specific to finding the script within the Arduino15 package structure.
    search_paths = []
    if sys.platform == "win32":
        local_app_data = os.environ.get('LOCALAPPDATA')
        if local_app_data:
            search_paths.append(os.path.join(local_app_data, 'Arduino15'))
    elif sys.platform == "darwin":
        search_paths.append(os.path.join(os.path.expanduser('~'), 'Library', 'Arduino15'))
    else: # Linux
        search_paths.append(os.path.join(os.path.expanduser('~'), '.arduino15'))

    for path in search_paths:
        esp_core_path = os.path.join(path, 'packages', 'esp32', 'hardware', 'esp32')
        if os.path.isdir(esp_core_path):
            versions = [d for d in os.listdir(esp_core_path) if os.path.isdir(os.path.join(esp_core_path, d))]
            if versions:
                latest_version = sorted(versions, reverse=True)[0]
                script_path = os.path.join(esp_core_path, latest_version, 'tools', 'espota.py')
                if os.path.exists(script_path):
                    return script_path
    return None

def compile_sketch(cli_path, fqbn, sketch_dir):
    """Compiles the sketch and returns the path to the generated .bin file."""
    if not os.path.isdir(sketch_dir):
        print(f"Error: Sketch directory not found at '{sketch_dir}'")
        return None

    sketch_name = os.path.basename(os.path.normpath(sketch_dir))
    
    with tempfile.TemporaryDirectory() as build_path:
        print(f"\n--- Compiling Sketch: {sketch_name} ---")
        command = [
            cli_path,
            'compile',
            '--fqbn', fqbn,
            '--output-dir', build_path,
            sketch_dir
        ]
        
        print(f"[DEBUG] Executing: {' '.join(command)}")
        process = subprocess.run(command, capture_output=True, text=True)
        
        if process.returncode != 0:
            print("--- Compilation Failed ---")
            print(process.stderr)
            return None
        
        print("--- Compilation Successful ---")
        
        # Find the generated .bin file and copy it to a persistent location
        binary_name = f"{sketch_name}.ino.bin"
        source_binary_path = os.path.join(build_path, binary_name)
        
        if os.path.exists(source_binary_path):
            # Copy to a known location (e.g., script's directory) to use after this function exits
            dest_binary_path = os.path.join(os.getcwd(), binary_name)
            shutil.copy(source_binary_path, dest_binary_path)
            return dest_binary_path
        else:
            print(f"Error: Could not find compiled binary '{binary_name}' in build path.")
            return None

def upload_firmware(espota_path, ip_address, file_path):
    """Uploads the firmware using espota.py."""
    print(f"\n--- Starting OTA Upload ---")
    print(f"  Target IP: {ip_address}")
    print(f"  Firmware:  {file_path}\n")

    command = [sys.executable, espota_path, '--ip', ip_address, '--file', file_path]
    
    try:
        process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, bufsize=1)
        for line in iter(process.stdout.readline, ''):
            print(line, end='')
        process.wait()
        
        if process.returncode == 0:
            print("\n--- OTA Upload Successful ---")
            return True
        else:
            print(f"\n--- OTA Upload Failed (Exit Code: {process.returncode}) ---")
            return False
    except Exception as e:
        print(f"An unexpected error occurred during upload: {e}")
        return False

def main():
    parser = argparse.ArgumentParser(description="Compile and upload an ESP32 sketch via OTA.")
    parser.add_argument('-s', '--sketch-dir', required=True, help="Path to the Arduino sketch directory.")
    parser.add_argument('-b', '--fqbn', required=True, help="Fully Qualified Board Name (e.g., esp32:esp32:esp32doit-devkit-v1).")
    parser.add_argument('-i', '--ip', required=True, help="IP address of the ESP32 device.")
    
    args = parser.parse_args()

    # 1. Find necessary tools
    arduino_cli_path = find_tool('arduino-cli')
    espota_script_path = find_espota()

    if not arduino_cli_path:
        print("Error: 'arduino-cli' not found in your system's PATH. Please install it and/or add it to your PATH.")
        sys.exit(1)
        
    if not espota_script_path:
        print("Error: Could not automatically find espota.py. Ensure the ESP32 board package is installed.")
        sys.exit(1)

    print(f"Found arduino-cli: {arduino_cli_path}")
    print(f"Found espota.py:   {espota_script_path}")

    # 2. Compile the sketch
    binary_path = compile_sketch(arduino_cli_path, args.fqbn, args.sketch_dir)

    if not binary_path:
        sys.exit(1)

    # 3. Upload the compiled binary
    if upload_firmware(espota_script_path, args.ip, binary_path):
        # 4. Clean up the copied binary file
        try:
            os.remove(binary_path)
            print(f"\nCleaned up temporary binary: {binary_path}")
        except OSError as e:
            print(f"Warning: Could not remove temporary binary file: {e}")
    
if __name__ == "__main__":
    main()