import fitz
import os
import argparse

"""
Args:
--name <output_filename.pdf> make sure to include the .pdf extension
--zoom <zoom_factor> float e.g. 1.13 enlarges/shrinks the slides. Useful if slides have invisible borders

Template is 39 x 56
1 (inner) Square is approximately 13.85 x 13.85
The (inner) starting point is (17.15, 18.075) (x,y)
Each line is 0.55 wide
"""


def ensure_directories_exist(input_folder, output_folder):
    if not os.path.exists(input_folder):
        os.makedirs(input_folder)
        print(f"Created directory: {input_folder}")
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)
        print(f"Created directory: {output_folder}")

def get_template_page(template_path):
    template_doc = fitz.open(template_path)
    template_page = template_doc.load_page(0)  # Assuming the template is a single page
    return template_doc, template_page

def get_pdf_aspect_ratio(folder_path):
    pdf_files = [f for f in os.listdir(folder_path) if f.lower().endswith('.pdf')]
    
    if not pdf_files:
        print("No PDF files found in the folder.")
        return None

    first_pdf = os.path.join(folder_path, pdf_files[0])

    doc = fitz.open(first_pdf)
    
    first_page = doc[0]
    
    rect = first_page.rect
    width = rect.width
    height = rect.height
    
    aspect_ratio = width / height
    doc.close()
    return aspect_ratio

def embed_slides_on_template(input_folder, output_folder, output_name, template_path, positions, zoom_factor, both_sides = False):
    if both_sides:
        slides_per_page=8
    else: 
        slides_per_page=4
    output_doc = fitz.open()  # Create a new output document
    template_doc = fitz.open(template_path)  # Load the template document

    for root, dirs, files in os.walk(input_folder):
        slide_count = 0
        for file in files:
            if file.endswith('.pdf'):
                doc = fitz.open(os.path.join(root, file))
                for page_num in range(len(doc)):
                    if slide_count % slides_per_page == 0:
                        output_page = output_doc.new_page(-1, width=template_doc[0].rect.width,
                                                          height=template_doc[0].rect.height)
                        output_page.show_pdf_page(output_page.rect, template_doc, 0)

                    pos = positions[slide_count % slides_per_page]
                    slide = doc.load_page(page_num)

                    # Compute zoomed rectangle
                    center_x = (pos[0] + pos[2]) / 2
                    center_y = (pos[1] + pos[3]) / 2
                    width = (pos[2] - pos[0])
                    height = (pos[3] - pos[1])
                    outline_rect = fitz.Rect(
                        center_x - width / 2,
                        center_y - height / 2,
                        center_x + width / 2,
                        center_y + height / 2
                    )
                    zoomed_rect = fitz.Rect(
                        center_x - width * zoom_factor / 2,
                        center_y - height * zoom_factor/ 2,
                        center_x + width * zoom_factor / 2,
                        center_y + height * zoom_factor / 2
                    )


                    output_page.show_pdf_page(zoomed_rect, doc, page_num)
                    #output_page.draw_rect(outline_rect, width=0.55)
                    slide_count += 1

                doc.close()

    output_pdf_path = os.path.join(output_folder, output_name)
    output_doc.save(output_pdf_path)
    print(f"Compiled notes saved to {output_pdf_path}")
    os.startfile(output_pdf_path)

"""
def draw_grid_on_template(template_path, output_path, aspect_ratio):
    doc = fitz.open(template_path)
    page = doc[0]  # Assuming you want to draw on the first page
    w, h = page.rect.width, page.rect.height

    # Grid parameters
    x0 = 17.15
    y0 = 18.075
    square_length = 13.85
    line_width = 0.55
    n_squares = 13

    add_y_pixels_slide = n_squares * square_length + (n_squares - 0.5) * line_width
    rect_slide = fitz.Rect(x0, y0, x0 + aspect_ratio * add_y_pixels_slide, y0 + add_y_pixels_slide)
    page.draw_rect(rect_slide, width=0.01)

    doc.save(output_path)
    os.startfile(output_path)
"""

def main():
    parser = argparse.ArgumentParser(description="Embed slides into a custom template.")
    parser.add_argument("--name", type=str, default="output_notes.pdf", help="Name of the output PDF file (include .pdf extension)")
    parser.add_argument("--zoom", type=float, default=1, help="Insert the aspect ratio of your slides. Default is 4/3")
    parser.add_argument("--both_sides", type=bool, default=False, help="True or False")
    args = parser.parse_args()

    output_name = args.name
    zoom = args.zoom
    both_sides = args.both_sides

    slides_per_page = 4  # Default 4

    base_dir = os.path.dirname(os.path.abspath(__file__))
    input_folder = os.path.join(base_dir, "../Input Folder")
    output_folder = os.path.join(base_dir, "../Output Folder")
    template_path = os.path.join(base_dir, "template.pdf")

    ensure_directories_exist(input_folder, output_folder)

    aspect_ratio = get_pdf_aspect_ratio(input_folder) # Default 4/3
    #aspect_ratio = 4/3
    template_doc = fitz.open(template_path)

    x0_left = 17.15  # Left margin starting x-coordinate
    y0 = 18.075
    square_length = 13.85
    line_width = 0.55
    n_squares = 13
    add_y_pixels_slide = n_squares * square_length + (n_squares - 1) * line_width
    x0_right = template_doc[0].rect.width - (x0_left + aspect_ratio * add_y_pixels_slide)

    y1 = y0 + add_y_pixels_slide + square_length + 2 * line_width
    y2 = y1 + add_y_pixels_slide + square_length + 2 * line_width
    y3 = y2 + add_y_pixels_slide + square_length + 2 * line_width

    positions = [
        (x0_left, y0, x0_left + aspect_ratio * add_y_pixels_slide, y0 + add_y_pixels_slide),  # Top-left
        (x0_left, y1, x0_left + aspect_ratio * add_y_pixels_slide, y1 + add_y_pixels_slide),  # Middle-left
        (x0_left, y2, x0_left + aspect_ratio * add_y_pixels_slide, y2 + add_y_pixels_slide),  # Bottom-left
        (x0_left, y3, x0_left + aspect_ratio * add_y_pixels_slide, y3 + add_y_pixels_slide),  # Bottom-most left
        (x0_right, y0, x0_right + aspect_ratio * add_y_pixels_slide, y0 + add_y_pixels_slide),  # Top-right
        (x0_right, y1, x0_right + aspect_ratio * add_y_pixels_slide, y1 + add_y_pixels_slide),  # Middle-right
        (x0_right, y2, x0_right + aspect_ratio * add_y_pixels_slide, y2 + add_y_pixels_slide),  # Bottom-right
        (x0_right, y3, x0_right + aspect_ratio * add_y_pixels_slide, y3 + add_y_pixels_slide),  # Bottom-most right
    ]

    if not args.both_sides:
        positions = positions[:4]
        
    embed_slides_on_template(input_folder, output_folder, output_name, template_path, positions, zoom, both_sides)

if __name__ == "__main__":
    main()
