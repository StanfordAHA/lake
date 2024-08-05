import os
import subprocess

def add_resulting_coll(all_lines, resulting_coll):
    joined_resulting_coll = " ".join(resulting_coll)
    joined_resulting_coll += "}]\n"

    all_lines.append(joined_resulting_coll)

def remove_appends(filename):
    with open(filename, 'r') as f:
        index = 0
        all_lines = []
        start_coll = False
        prev_line_append = False

        for line in f:
            if "set __coll_" == line[:11]:

                if prev_line_append:
                    add_resulting_coll(all_lines, resulting_coll)

                start_coll = True
                resulting_coll = [line.split("}]")[0]]

                prev_line_append = False

            elif "append_to_collection" in line:

                print(index, "append line")
                if "get_pins {" in line:
                    next_part = (line.split("get_pins {"))[1]
                elif "get_ports {" in line:
                    next_part = (line.split("get_ports {"))[1]
                else:
                    assert False, f"Error: Unsupported append string {line}"

                resulting_coll.append(next_part.split("}]")[0])

                prev_line_append = True

            elif start_coll:

                print(index, "joined line")

                add_resulting_coll(all_lines, resulting_coll)

                all_lines.append(line)

                start_coll = False
                prev_line_append = False

            else:

                print(index, "normal line")

                all_lines.append(line)

                prev_line_append = False

            index += 1

    with open("outputs/new.pt.sdc", 'w') as f:
        for line in all_lines:
            f.write(line)

    return None


if __name__ == "__main__":
    print(os.getcwd())
    filename = "outputs/design.pt.sdc"

    remove_appends(filename)

