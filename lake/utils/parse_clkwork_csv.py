'''This file contains functions for:
    - parsing outputs from Clockwork for the
      specified input format for the SMT solver
    - parsing outputs from Clockwork for lists
      of expected format for testbench in
      Garnet / Lake for the memory tile
'''

import csv


# example:
# width = 4 returns [0 0 0 0]
def zero_arr(width):
    data0 = '['
    for i in range(width):
        data0 += '0 '
    data0 = data0[0:-1] + ']'
    return data0


# example:
# string = '[10 20 30 40]' returns '[[10], [20], [30], [40]]'
# TODO clean up this code
def format_comma_bracket(string):
    # makes sure numbers are grouped together
    # string splits up into digits, but we want to support more than
    # just single-digit numbers
    l = list(string)
    newl = list(string)
    # ni is amount that has been changed as a result of changes to newl
    # it needs to be added to index through l
    ni = 0
    for i in range(len(l)):
        if i > 0:
            try:
                # group same number together
                if isinstance(int(l[i - 1]), int) and isinstance(int(l[i]), int):
                    new_index = i + ni - 1
                    # new_index is index for grouped together number
                    newl[new_index] = l[i - 1] + l[i]
                    # delete the digit appended to the number
                    newl.pop(new_index + 1)
                    # off from i by 1 now
                    ni -= 1
            except ValueError:
                pass

    # all digits grouped together now
    l = newl[:]
    # this is the index before a number in newl
    ni = 0
    for i in range(len(l)):
        char = l[i]
        if i == 1:
            ni = 1
        try:
            if isinstance(int(char), int):
                # insert before number
                newl.insert(ni, '[')
                # do not insert comma if at end
                if l[i + 1] == ']':
                    # appends have to be done after before number + number = 2
                    newl.insert(ni + 2, ']')
                    # added one element before and one element after
                    ni += 2
                else:
                    newl.insert(ni + 2, '],')
                    ni += 2
        except ValueError:
            pass
        ni += 1
    new_string = ""
    # put the string back together
    for item in newl:
        new_string += item
    return new_string


# converts rd@/wr@ and data_in=/data_out= format to
# consumable format for SMT
# output is _parse.csv file
def parse(csv_file_name,
          data_in_width,
          data_out_width,
          data_in_name="data_in",
          data_out_name="data_out"):

    with open(csv_file_name[:-4] + '_parse.csv', 'w') as parsefile:
        filewriter = csv.writer(parsefile, delimiter=',')
        # file headings
        # for no cycle number for now
        # filewriter.writerow(['cycle num', 'data_in', 'data_out'])
        filewriter.writerow([data_in_name, data_out_name])
        # input file
        csv_file = open(csv_file_name, "r")
        reader = csv.reader(csv_file, delimiter=',')

        # zeros for input / output data when there is no write / read
        data_in0 = zero_arr(data_in_width)  # format_comma_bracket(zero_arr(data_in_width))
        data_out0 = zero_arr(data_out_width)  # format_comma_bracket(zero_arr(data_out_width))

        # TODO clean this up -> issues with initializing array?
        data = []
        for row in reader:
            # there is a read and write on this cycle
            # format_row = format_comma_bracket(row[1][5:])
            format_row = row[1][5:]
            if len(data) > 0 and \
                    (int(data[len(data) - 1][0]) == int(row[0][3:])):
                if row[0][0:2] == "rd":
                    data[len(data) - 1][2] = format_row
                else:
                    data[len(data) - 1][1] = format_row
            else:
                if row[0][0:2] == "rd":
                    data.append([row[0][3:], data_in0, format_row])
                else:
                    data.append([row[0][3:], format_row, data_out0])

        # add rows with no ops that do not have a cycle count
        i = 0
        prev_dat = -1
        while i < len(data):
            dat = data[i]
            if int(dat[0]) == prev_dat + 1:
                filewriter.writerow(dat[1:])
                # for no cycle number for now
                # filewriter.writerow(dat)
                # break condition
                i += 1
                prev_dat = int(dat[0])
            else:
                prev_dat += 1
                filewriter.writerow([data_in0, data_out0])
                # for no cycle number for now
                # filewriter.writerow([str(prev_dat), data_in0, data_out0])


# csv file for test bench headings
# example: data_width = 3, data_name = data_in
#  will generate headings data_in_0, data_in_1, data_in_2
def create_tb_headings(data_width, data_name):
    headings = []
    if data_width == 1:
        headings.append(data_name)
    else:
        for i in range(data_width):
            headings.append(data_name + f'_{i}')
    return headings


# makes a csv with columns as fault test bench names
# like data_in_0, data_in_1, data_in_2
def read_parsed(csv_file_name,
                data_in_width,
                data_out_width,
                data_in_name,
                data_out_name):

    with open(csv_file_name[:-4] + '_tb.csv', 'w') as tbfile:
        filewriter = csv.writer(tbfile, delimiter=',')

        filewriter.writerow(create_tb_headings(data_in_width, data_in_name) +
                            create_tb_headings(data_out_width, data_out_name))

        csv_file = open(csv_file_name, "r")
        reader = csv.reader(csv_file, delimiter=',')

        # skip first row with headings
        start = False
        for row in reader:
            row_in = row[0].replace('[', '').replace(']', '').split()
            row_out = row[1].replace('[', '').replace(']', '').split()
            if start:
                write_row = []
                for i in range(data_in_width):
                    write_row.append(row_in[i])
                for j in range(data_out_width):
                    write_row.append(row_out[j])
                filewriter.writerow(write_row)
            start = True


# parses file and outputs a csv with tb format names
def parse_and_tb(csv_file_name,
                 data_in_width,
                 data_out_width,
                 data_in_name="data_in",
                 data_out_name="data_out"):

    parse(csv_file_name,
          data_in_width,
          data_out_width,
          data_in_name,
          data_out_name)

    read_parsed(csv_file_name[:-4] + '_parse.csv',
                data_in_width,
                data_out_width,
                data_in_name,
                data_out_name)


# gets wide data for the SRAM
def append_shift(data, data_width, row, bit_width):
    dat = 0
    for i in range(data_width):
        dat = dat | (int(row[i]) << i * bit_width)
    data.append(dat)


# returns lists for data_in and data_out sequences
# input is a _parse.csv file
def generate_data_lists(csv_file_name,
                        data_in_width,
                        data_out_width,
                        data_in_name="data_in",
                        data_out_name="data_out",
                        bit_width=16,
                        is_wide=False):

    csv_file = open(csv_file_name, "r")
    reader = csv.reader(csv_file, delimiter=',')

    valids = []
    if is_wide:
        in_data, out_data = [], []
    else:
        in_data = [[] for _ in range(data_in_width)]
        out_data = [[] for _ in range(data_out_width)]
    # skip first row with headings
    start = False
    in_index = 0
    out_index = 1
    has_valid = False
    valid_index = None
    for row in reader:
        if not start:
            for i in range(len(row)):
                col = row[i]
                col = col.replace(' ', '')
                if col == "data_in":
                    in_index = i
                elif col == "data_out":
                    out_index = i
                elif col == "valid_out":
                    valid_index = i
                    has_valid = True

        row_in = row[in_index].replace('[', '').replace(']', '').split()
        row_out = row[out_index].replace('[', '').replace(']', '').split()

        if start:
            if has_valid:
                row_valid = row[valid_index]
                valids.append(int(row_valid))

            if is_wide:
                append_shift(in_data[0], data_in_width, row_in, bit_width)
                append_shift(out_data[0], data_out_width, row_out, bit_width)
            else:
                for i in range(data_in_width):
                    try:
                        in_data[i].append(int(row_in[i]))
                    except:
                        pass
                for j in range(data_out_width):
                    try:
                        out_data[j].append(int(row_out[j]))
                    except:
                        pass
            # else:
                # copy code from read_parsed
        start = True
    # print({data_in_name: in_data, data_out_name: out_data})
    # return {data_in_name: in_data, data_out_name: out_data}
    if is_wide:
        return (in_data[0], out_data[0])

    return (in_data, out_data, valids)


def parse_and_lists(csv_file_name,
                    data_in_width,
                    data_out_width,
                    data_in_name,
                    data_out_name,
                    bit_width=16,
                    is_wide=True):

    parse_and_tb(csv_file_name=csv_file_name,
                 data_in_width=data_in_width,
                 data_out_width=data_out_width,
                 data_in_name=data_in_name,
                 data_out_name=data_out_name)

    generate_data_lists(csv_file_name[:-4] + '_parse.csv',
                        data_in_width,
                        data_out_width,
                        data_in_name,
                        data_out_name,
                        bit_width=bit_width,
                        is_wide=is_wide)


if __name__ == "__main__":
    # parse_and_tb(csv_file_name='buf_agg_SMT.csv',
    #              data_in_width=1,
    #              data_out_width=4,
    #              data_in_name="data_in",
    #              data_out_name="data_out")

    # parse_and_lists('buf_sram_SMT.csv', 4, 4, "data_in", "data_out", 16, True)
    generate_data_lists('test.csv', 2, 2)
    # parse_and_tb(csv_file_name='buf_tb_SMT.csv',
    #              data_in_width=4,
    #              data_out_width=1)
