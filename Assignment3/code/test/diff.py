import sys
from difflib import unified_diff
import glob


def diff(file1, file2):
	with open(file1, 'r') as f1:
		with open(file2, 'r') as f2:
			diff_gen = list(unified_diff(f1.read(), f2.read(), file1, file2))
			if not diff_gen:
				return
			else:
				text = "\n".join(diff_gen)
				f_name = file1.split('.')[0]
				diff_format(f_name, text)

def diff_format(f_name, text_diff):
	file_print = '*  ' + f_name + '  *'
	star_print = '*' * len(file_print)
	if text_diff:
		print(star_print)
		print(file_print)
		print(star_print)

		print(text_diff)
		
		print(star_print)

		print("\n")

def diff_all():
	all_sim = set([i.split('.sim')[0] for i in glob.iglob('*.sim')])
	all_output = set([i.split('.output')[0] for i in glob.iglob('*.output')])
	test_files = all_sim & all_output
	for f_name in test_files:
		diff(f_name + '.sim', f_name + '.output')

if __name__ == "__main__":
	diff(sys.argv[1], sys.argv[2])