#coding:utf-8
# python实现pdf格式转换图片格式
# https://blog.csdn.net/stf1065716904/article/details/83751437
#
#-----------------------------------------------------------------------------------------------------------------------
#
# https://stackoverflow.com/questions/2693820/extract-images-from-pdf-without-resampling-in-python
# Extract images from PDF without resampling, in python?
# If there is an error:
#   File "D:\Programs\Python\Python38\lib\site-packages\PyPDF2\filters.py", line 363, in decodeStreamData
#     raise NotImplementedError("unsupported filter %s" % filterType)
# Download new PyPDF2 from
# https://github.com/sylvainpelissier/PyPDF2
#
# copy PyPDF2\* to File "D:\Programs\Python\Python38\lib\site-packages\PyPDF2
#
#-----------------------------------------------------------------------------------------------------------------------
#
import io
import sys
import os
import getopt
import glob
import shutil
from wand.image import Image
from wand.color import Color
from PIL import Image as PImage
from PyPDF2 import PdfFileReader, PdfFileWriter

memo = {}

def ResizeImage(fileinp, fileout, width, height, type):
	img = PImage.open(fileinp)
	width0  = img.size[0]
	height0 = img.size[1]
	ratio0  = height0 / width0
	if width > 0 and height < 0:
		width1  = width
		height1 = int(round(ratio0 * width1,0))
	print(width1, height1)
	out = img.resize((width1, height1), PImage.ANTIALIAS)  # resize image with high-quality
	out.save(fileout, type)

def getPdfReader(filename):
	reader = memo.get(filename, None)
	if reader is None:
		reader = PdfFileReader(filename, strict=False)
		memo[filename] = reader
	return reader

def _run_convert(pdfile, savedfilename, page_index, index, res=240):
	# http://www.imooc.com/wenda/detail/600387
	# 在Python中从PDF提取图像而无需重新采样？
	#
	pageObj = pdfile.getPage(page_index)#获取pdf的第page_index页
	dst_pdf = PdfFileWriter()
	dst_pdf.addPage(pageObj)
	pdf_bytes = io.BytesIO()
	dst_pdf.write(pdf_bytes)
	pdf_bytes.seek(0)
	img = Image(file=pdf_bytes, resolution=res)
	img.format = 'png'
	img.compression_quality = 100
	img.background_color = Color("white")
	number=index+1
	img_path = '%s页面_%02d.jpg' % (savedfilename, number)
	#-------------------------------------------------------------------------------------------------------------------
	# Save Images
	#
	img.save(filename=img_path)
	print("extract "+img_path)
	img.destroy()

def dealPerPdf(workdir, path, file, index):
	savedfilename = path.split('/')[-1].split('-')[0] + '_'
	new_path = os.path.join(path, file)
	pdfile = getPdfReader(new_path)      # 打开pdf文件句柄
	#-------------------------------------------------------------------------------------------------------------------
	# page0 = pdfile.getPage(1)
	# xObject = page0['/Resources']['/XObject'].getObject()
	# for obj in xObject:
	# 	if xObject[obj]['/Subtype'] == '/Image':
	# 		size = (xObject[obj]['/Width'], xObject[obj]['/Height'])
	# 		print("size======================================")
	# 		print(xObject[obj])
	# 		print(obj)
	# 		print(size)
	# 		print("size======================================")
	# 		data = xObject[obj].getData()
	# 		if xObject[obj]['/ColorSpace'] == '/DeviceRGB':
	# 			mode = "RGB"
	# 		else:
	# 			mode = "P"
	# 		if xObject[obj]['/Filter'] == '/FlateDecode':
	# 			img = Image.frombytes(mode, size, data)
	# 			img.save(obj[1:] + ".png")
	# 		elif xObject[obj]['/Filter'] == '/DCTDecode':
	# 			img = open(obj[1:] + ".jpg", "wb")
	# 			img.write(data)
	# 			img.close()
	# 		elif xObject[obj]['/Filter'] == '/JPXDecode':
	# 			img = open(obj[1:] + ".jp2", "wb")
	# 			img.write(data)
	# 			img.close()
	# exit(1)
	# -------------------------------------------------------------------------------------------------------------------
	page_nums = pdfile.getNumPages()     # 获取pdf总页数
	os.chdir(path)
	for page_index in range(page_nums):
		_run_convert(pdfile, savedfilename, page_index, index)
		index = index + 1
	os.chdir(workdir)
	return index

def getAllfiles(path):
	files = os.listdir(path)
	files.sort()
	index = 0
	for file in files:
		new_path = path + '/' + file;
		if os.path.isdir(new_path):
			getAllfiles(new_path)
		elif os.path.isfile(new_path):
			is_pdf = file.split('.')[-1]
			if is_pdf != 'pdf':
				continue
			index = dealPerPdf(path, file, index)
			index = index+1
 
def DealBatchPdf(path):
	getAllfiles(path)

def expdf(inputfile, extractImage, trimWhiteBoarder):
	#-------------------------------------------------------------------------------------------------------------------
	fname=os.path.splitext(inputfile)
	filename = os.path.split(fname[0])
	dirname=filename[1]
	workdir=os.getcwd()
	#-------------------------------------------------------------------------------------------------------------------
	if not os.path.exists(dirname):
		os.makedirs(dirname)
	#-------------------------------------------------------------------------------------------------------------------
	newfile=dirname+"/"+inputfile
	shutil.copyfile(inputfile, newfile)
	path=dirname
	is_batch_deal = False
	if extractImage == 1:
		if is_batch_deal:
			DealBatchPdf(path)
		else:
			filename = inputfile #要处理的pdf文件名
			dealPerPdf(workdir, path, filename, 0)
	#-------------------------------------------------------------------------------------------------------------------
	width0  = 1920
	height0 = 1080
	# -------------------------------------------------------------------------------------------------------------------
	if trimWhiteBoarder == 1:
		if is_batch_deal:
			print("Not implemented...")
		else:
			os.chdir(path)
			datanames = os.listdir(os.getcwd())
			for dataname in datanames:
				if os.path.splitext(dataname)[1] == '.jpg':
					#---------------------------------------------------------------------------------------------------
					# ResizeImage(fileinp=dataname, fileout=tmpjpg1, width=1920, height=-1, type="png")
					#---------------------------------------------------------------------------------------------------
					cmd="convert "+dataname+" -resize "+str(width0)+" "+dataname
					print(cmd)
					os.system(cmd)
					#---------------------------------------------------------------------------------------------------
					img    = PImage.open(dataname)
					width  = img.size[0]
					height = img.size[1]
					delta  = int(round((height - height0) / 2, 0))
					#---------------------------------------------------------------------------------------------------
					cmd="convert "+dataname+" -shave 0x"+str(delta)+" "+dataname
					print(cmd)
					os.system(cmd)
					#---------------------------------------------------------------------------------------------------
					# img = PImage.open(tmpjpg2)
					# width = img.size[0]
					# height = img.size[1]
					# print(width, height)
					#---------------------------------------------------------------------------------------------------
			os.chdir(workdir)
	if os.path.exists(newfile):
		os.remove(newfile)
	# shutil.copyfile(inputfile, newfile)
	#-------------------------------------------------------------------------------------------------------------------