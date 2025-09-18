from PIL import Image
img1 = Image.open("map_Anaemia_27_08_2025.png")
img2 = Image.open("map_FPro.png")
img3 = Image.open("map_Anaemia_Prediction_all_27_08_2025_confounders.png")
img4 = Image.open("map_MAE_Anaemia_ORANGE_all_27_08_2025_confounders.png")

img1 = img1.crop((50, 50, img1.width - 50, img1.height - 50))
img2 = img2.crop((50, 50, img2.width - 50, img2.height - 50))
img3 = img3.crop((50, 50, img3.width - 50, img3.height - 50))
img4 = img4.crop((50, 50, img4.width - 50, img4.height - 50))
min_height = min(img1.height, img2.height, img3.height, img4.height)
def resize_to_height(img, height):
	w_percent = height / img.height
	new_width = int(img.width * w_percent)
	return img.resize((new_width, height), Image.LANCZOS)

img1 = resize_to_height(img1, min_height)
img2 = resize_to_height(img2, min_height)
img3 = resize_to_height(img3, min_height)
img4 = resize_to_height(img4, min_height)
total_width = img1.width + img2.width
total_height = img1.height+ img3.height
new_img = Image.new("RGB", (total_width, total_height))
new_img.paste(img1, (0, 0))
new_img.paste(img2, (img1.width, 0))
new_img.paste(img3, (0, img1.height))
new_img.paste(img4, (img1.width, img1.height))

new_img.save("03_09_2025_2x2_Anaemia-FPro-Pred_Anaemia-MAE_residuals.png")


from PIL import Image, ImageDraw, ImageFont

img = Image.open("03_09_2025_2x2_Anaemia-FPro-Pred_Anaemia-MAE_residuals.png")
draw = ImageDraw.Draw(img)

# Optional: choose a font and size (default may be too small)
font = ImageFont.truetype("DejaVuSans.ttf", 130)

# Get image dimensions
w, h = img.size

# Assuming 2x2 grid
half_w, half_h = w // 2, h // 2

# Coordinates for top-left corners of each subfigure
positions = [
    (10, 10),                 # top-left (a)
    (half_w + 10, 10),        # top-right (b)
    (10, half_h + 10),        # bottom-left (c)
    (half_w + 10, half_h + 10) # bottom-right (d)
]

labels = ["(A)", "(B)", "(C)", "(D)"]

# Draw labels
for pos, label in zip(positions, labels):
    draw.text(pos, label, fill="black", font=font)  # use "white" if background is dark

# Save output
img.save("03_09_2025_2x2_Anaemia-FPro-Pred_Anaemia-MAE_residuals_labeled.png")
 
