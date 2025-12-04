#!/usr/bin/env python3
import pypdf
import sys
from pathlib import Path

def split_pdf(input_file, pages_per_chunk=10):
    """Split a PDF into smaller chunks"""
    input_path = Path(input_file)

    # Read the PDF
    reader = pypdf.PdfReader(input_path)
    total_pages = len(reader.pages)

    print(f"Total pages: {total_pages}")
    print(f"Splitting into chunks of {pages_per_chunk} pages each...")

    # Create output directory
    output_dir = Path("pdf_chunks")
    output_dir.mkdir(exist_ok=True)

    # Split the PDF
    chunk_num = 1
    for start_page in range(0, total_pages, pages_per_chunk):
        writer = pypdf.PdfWriter()
        end_page = min(start_page + pages_per_chunk, total_pages)

        # Add pages to this chunk
        for page_num in range(start_page, end_page):
            writer.add_page(reader.pages[page_num])

        # Write the chunk
        output_file = output_dir / f"chunk_{chunk_num:03d}_pages_{start_page+1}-{end_page}.pdf"
        with open(output_file, "wb") as output_pdf:
            writer.write(output_pdf)

        print(f"Created: {output_file} ({end_page - start_page} pages)")
        chunk_num += 1

    print(f"\nSuccessfully split PDF into {chunk_num-1} chunks in '{output_dir}' directory")
    return chunk_num - 1

if __name__ == "__main__":
    split_pdf("CSNM Notes.pdf", pages_per_chunk=5)