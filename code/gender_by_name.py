import os
import io 
import sys


class gender():
	def __init__(self, indir, outdir):
		self.dir = indir
		self.outdir = outdir
		self.names_female = {}
		self.names_male = {}

	def get_names_counts(self):
		'''
			The names for each year are listed in a file.
			Read all the files and combine the info from all the years.
			names_female and names_male includes the names and 
			their counts as Female or Male in the Census data.
			A name can be listed in both dictionaries. 
		'''
		files = os.listdir(self.dir)
		for file in files:
			f = open(self.dir + "/" + file, "rw+")
			print "file: ", f.name
			lines = f.readlines()
			for line in lines:
				words = line.split(",")
				firstname = words[0].strip().lower()
				male_female = words[1].strip()
				count = int(words[2].strip())
				#print (firstname + ", " + male_female + ", " + str(count))
				#count = 0
				if male_female == "F":
					if firstname in self.names_female.keys():
						#print (firstname + ", " + str(count))
						count += self.names_female[firstname]
					self.names_female[firstname] = count
					#print (firstname + ", " + str(count))
				else:
					if firstname in self.names_male.keys():
						#print (firstname + ", " + str(count))
						count += self.names_male[firstname] 
					self.names_male[firstname] = count
					#print (firstname + ", " + str(count))

		f.close()

	def get_gender(self):
		'''
			After running get_names_counts() and getting the counts for each name, 
			we label a name as female or male based on their appearance in 
			names_female and names_male dictionaries. 
			Creates three files as an output: 
			ambiguous-names.txt, female-1900-2013.txt and male-1900-2013.txt.
		'''

		print("Female Names: " + str(len(names_female.keys())))
		print("Male Names: " + str(len(names_male.keys())))
		overlap = open(self.outdir + "/ambiguous-names.txt", "wb")
		female = open(self.outdir + "/female-1900-2013.txt", "wb")
		for name in names_female:
			percentage = 1
			if name in names_male.keys():
				female_count = names_female[name]
				male_count = names_male[name]
				percentage = female_count/float(female_count + male_count)
				#print(percentage)
				overlap.write(name +", " + "%.2f" %percentage +"\n")
			if percentage >= 0.95:
				female.write(name + "\n")
		female.close()


		male = open(self.outdir + "/male-1900-2013.txt", "wb")
		for name in names_male:
			percentage = 1
			if name in names_female.keys():
				female_count = names_female[name]
				male_count = names_male[name]
				percentage = male_count/float(female_count + male_count)
				overlap.write(name + ": " + "%.2f" %percentage +"\n")
			if percentage >= 0.95:
				male.write(name+ "\n")
		male.close()

		overlap.close()

	def get_quantified_gender(self):
		'''
			Computes a probablity for a name to be Male. 
			If it is equal to 1, it means the name has been used for males.
			If it is equal to 0, it means the name has been used for females.
		'''
		for name in self.names_female:
			print name,
		outfile = open(self.outdir + "/quantified_gender_1900_2013.txt", "wb")
		names = list(set(self.names_female.keys()) | set(self.names_male.keys()))
		#names.add(self.names_female.keys())
		#names.add(self.names_male.keys())
		for name in names:
			percentage = 0
			female_count = 0
			male_count = 0
			if name in self.names_female.keys():
				female_count = self.names_female[name]
			if name in self.names_male.keys():	
				male_count = self.names_male[name]
			percentage = male_count/float(female_count + male_count)
			outfile.write(name + ": " + "%.2f" %percentage +"\n")
			
		outfile.close()


if __name__ == "__main__":
	obj = gender(sys.argv[1], sys.argv[2])
	obj.get_names_counts()
	obj.get_gener()
	obj.get_quantified_gender()

