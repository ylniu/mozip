import extract_image_from_pdf
import sys
import getopt

if __name__ == '__main__':
	inputfile = ''
	outputfile = ''
	extractImage=1
	trimWhiteBoarder=1
	try:
		opts, args = getopt.getopt(sys.argv[1:],"hi:o:",["ifile=","ofile="])
	except getopt.GetoptError:
		print ('test.py -i <inputfile> -o <outputfile>')
		sys.exit(2)
	for opt, arg in opts:
		if opt == '-h':
			print ('test.py -i <inputfile> -o <outputfile>')
			sys.exit()
		elif opt in ("-i", "--ifile"):
			inputfile = arg
		elif opt in ("-o", "--ofile"):
			outputfile = arg
	extract_image_from_pdf.expdf(inputfile, extractImage, trimWhiteBoarder)