import os
import subprocess

def list_files_in_folder(folder_path):
    """
    Lists all the files in the given folder.

    Parameters:
        folder_path (str): The path to the folder.

    Returns:
        list: A list of file names in the folder.
    """
    try:
        # Get the list of all files and directories in the folder
        files_and_dirs = os.listdir(folder_path)
        
        # Filter out directories, keep only files
        files = [f for f in files_and_dirs if os.path.isfile(os.path.join(folder_path, f))]
        return files
    except Exception as e:
        print(f"Error: {e}")
        return []
    
def gzip_files_in_folder(folder_path, output_folder_path):
    """
    Gzip all files in the folder and store them in a 'gziped' folder without the .gz extension.

    Parameters:
        folder_path (str): The path to the folder.
    """
    gziped_folder = output_folder_path

    # Create the gziped folder if it doesn't exist
    os.makedirs(gziped_folder, exist_ok=True)

    files = list_files_in_folder(folder_path)

    for file in files:
        original_file_path = os.path.join(folder_path, file)
        gziped_file_path = os.path.join(gziped_folder, file.replace(".json","") )
        print(original_file_path, gziped_file_path)
        # Run the gzip command with -k to keep the original file
        try:
            subprocess.run(["gzip", "-k", original_file_path], check=True)

        #     # Move the .gz file to the gziped folder and rename without .gz extension
            gz_file_path = original_file_path + ".gz"
            os.rename(gz_file_path, gziped_file_path)
        #     print(f"Gzipped and moved: {file}")
        except subprocess.CalledProcessError as e:
            print(f"Failed to gzip file: {file}. Error: {e}")


if __name__ == "__main__":
    # folder_path = input("Enter the folder path: ")
    # files = list_files_in_folder(os.path.dirname(__file__) + "/countriesAndStates")

    # if files:
    #     print("\nFiles in the folder:")
    #     for file in files:
    gzip_files_in_folder(os.path.dirname(__file__) + "/countriesAndStates-Jsons", os.path.dirname(__file__) + "/" + "countriesAndStates")
    # else:
    #     print("\nNo files found or invalid folder path.")