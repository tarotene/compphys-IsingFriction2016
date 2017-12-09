from PIL import Image
img = Image.open('sample.jpg')
gray_img = img.convert('L')
gray_img.save('sample_mono.jpg')
