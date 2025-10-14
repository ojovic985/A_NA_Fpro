from PIL import Image
img1 = Image.open("US-NHANES.png")
img2 = Image.open("test_KHANES.png")
img3 = Image.open("test_UKNDNS.png")

img1_2 = img1.crop((135, 75, img1.width - 210, img1.height - 310))
img2_2 = img2.crop((135, 65, img2.width - 145, img2.height - 310))
img3_2 = img3.crop((135, 65, img3.width - 155, img3.height - 310))

img1=img1_2
img2=img2_2
img3=img3_2

min_height = min(img1.height, img2.height, img3.height)
def resize_to_height(img, height):
	w_percent = height / img.height
	new_width = int(img.width * w_percent)
	return img.resize((new_width, height), Image.LANCZOS)

img1 = resize_to_height(img1, min_height)
img2 = resize_to_height(img2, min_height)
img3 = resize_to_height(img3, min_height)

total_width = img1.width
total_height = img1.height+ img2.height+ img3.height
new_img = Image.new("RGB", (total_width, total_height))
new_img.paste(img1, (0, 0))
new_img.paste(img2, (0, img1.height))
new_img.paste(img3, (0, img1.height+img2.height))
new_img.save("US_K_NHANES_and_UKNDNS.png")



from PIL import Image, ImageDraw, ImageFont
import os

img = Image.open("US_K_NHANES_and_UKNDNS.png")
draw = ImageDraw.Draw(img)

# Optional: choose a font and size (default may be too small)
font = ImageFont.truetype("DejaVuSans.ttf", 130)

# Get image dimensions
w, h = img.size

# Assuming 1x3 grid
first_third_h, second_third_h = h // 3, 2*h // 3

# Coordinates for top-left corners of each subfigure
positions = [
    (5, 80),                 # top (a)
    (5, first_third_h + 80),        # middle (b)
    (5, second_third_h + 80),        # low (c)
]

labels = ["(A)", "(B)", "(C)"]

# Draw labels
for pos, label in zip(positions, labels):
    draw.text(pos, label, fill="black", font=font)  # use "white" if background is dark

# Save output
img.save("Figure_1.png")


file_name = "US_K_NHANES_and_UKNDNS.png"

if os.path.isfile(file_name):
    os.remove(file_name)
    print(f"{file_name} deleted.")
else:
    print(f"{file_name} does not exist.")


 
