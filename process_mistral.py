"""Extract text from PDFs using the Mistral API"""

from glob import glob
from os import getenv, makedirs
from os.path import basename, dirname, isfile

from dotenv import load_dotenv
from mistralai import Mistral

if __name__ == "__main__":
    files = glob("reports/raw/*/*.pdf")

    load_dotenv()
    client = Mistral(api_key=getenv("MISTRAL_API_KEY"))

    for file in files:
        name = basename(file)
        out_dir = dirname(file).replace("raw", "mistral")
        makedirs(out_dir, exist_ok=True)
        out_file = out_dir + "/" + name.replace("pdf", "json")

        if not isfile(out_file):
            print(f"processing {file}")
            with open(file, "rb", encoding="utf-8") as content:
                uploaded_pdf = client.files.upload(
                    file={"file_name": name, "content": content},
                    purpose="ocr",
                )
            signed_url = client.files.get_signed_url(file_id=uploaded_pdf.id)
            ocr_response = client.ocr.process(
                model="mistral-ocr-latest",
                document={"type": "document_url", "document_url": signed_url.url},
            )
            with open(out_file, "w", encoding="utf-8") as file:
                file.write(ocr_response.model_dump_json())
