import os
import json

def load_json(file_path):
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            return json.load(f)
    except json.JSONDecodeError:
        print(f"Error decoding JSON in file: {file_path}")
        return None

def verify_keys(directory):
    en_json_path = os.path.join(directory, "en.json")
    print(en_json_path)
    if not os.path.exists(en_json_path):
        print("Error: en.json not found in the directory.")
        return
    
    en_keys = set(load_json(en_json_path).keys())
    
    for file_name in os.listdir(directory):
        if file_name.endswith(".json") and file_name != "en.json":
            file_path = os.path.join(directory, file_name)
            json_data = load_json(file_path)
            if json_data is None:
                continue
            
            other_keys = set(json_data.keys())
            missing_keys = en_keys - other_keys
            
            if missing_keys:
                print(f"{file_name} is missing keys: {missing_keys}")
            # else:
            #     print(f"{file_name} contains all keys from en.json")

if __name__ == "__main__":
    directory = os.path.join(os.getcwd(), "LocaleStrings","jsons")
    verify_keys(directory)
