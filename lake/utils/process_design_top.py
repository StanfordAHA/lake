import argparse
import json
import kratos

# allowable_configs = ['sram2tb_0',
#                      'sram2tb_1',
#                      'tb2out_0',
#                      'tb2out_1']
allowable_configs = ['agg2sram_0',
                     'agg2sram_1',
                     'in2agg_0',
                     'in2agg_1',
                     'sram2tb_0',
                     'sram2tb_1',
                     'tb2out_0',
                     'tb2out_1']

# pairs = [('sram2tb_0', 'agg2sram_0'),
#          ('sram2tb_0', 'agg2sram_1'),
#          ('sram2tb_1', 'agg2sram_0'),
#          ('sram2tb_1', 'agg2sram_1'),
#          ('tb2out_0', 'sram2tb_0'),
#          ('tb2out_0', 'sram2tb_1'),
#          ('tb2out_1', 'sram2tb_0'),
#          ('tb2out_1', 'sram2tb_1'),]
pairs = [('sram2tb_0', ['agg2sram_0', 'agg2sram_1', 'in2agg_0', 'in2agg_1']),
         ('sram2tb_1', ['agg2sram_0', 'agg2sram_1', 'in2agg_0', 'in2agg_1']),
         ('tb2out_0', ['sram2tb_0', 'sram2tb_1', 'agg2sram_0', 'agg2sram_1', 'in2agg_0', 'in2agg_1']),
         ('tb2out_1', ['sram2tb_0', 'sram2tb_1', 'agg2sram_0', 'agg2sram_1', 'in2agg_0', 'in2agg_1']), ]
# pairs = [('sram2tb_0', 'in2agg_0'),
#          ('sram2tb_1', 'in2agg_0'),
#          ('sram2tb_0', 'in2agg_1'),
#          ('sram2tb_1', 'in2agg_1')]

VBOSE = False


class SequenceIterator():

    def __init__(self,
                 dimensionality,
                 strides,
                 ranges,
                 start) -> None:
        self.dimensionality = dimensionality
        self.ctrs = [0 for _ in range(dimensionality)]
        self.strides = strides
        self.ranges = ranges
        self.start = start
        self.done = False

    def initialize(self):
        self.ctrs = [0 for _ in range(self.dimensionality)]
        self.done = False

    def get_next_event(self):

        event = self.start

        for i_ in range(self.dimensionality):
            event += self.ctrs[i_] * self.strides[i_]

        # self.iterate()

        return event

    def iterate(self):

        increment_next = True

        for i_ in range(self.dimensionality):
            if increment_next:
                self.ctrs[i_] += 1
                # If it hits the boundary, set it back to 0 and increment the next one
                if self.ctrs[i_] == self.ranges[i_]:
                    self.ctrs[i_] = 0
                    increment_next = True
                    if i_ == (self.dimensionality - 1):
                        # This condition is the end
                        self.done = True
                else:
                    increment_next = False
            else:
                break

        return self.done


def get_num_events(from_cfg=None, stop=0):

    # print(from_cfg)
    # print(stop)
    # print('config')
    # print(from_cfg)
    # print(stop)

    # print(from_cfg)

    si = SequenceIterator(dimensionality=from_cfg['dimensionality'],
                          strides=from_cfg['cycle_stride'],
                          ranges=from_cfg['extent'],
                          start=from_cfg['cycle_starting_addr'][0])

    si.initialize()
    num_events = 0

    event_ = 0
    is_done = False

    while si.get_next_event() < stop and not is_done:
        event_ = si.get_next_event()
        # print(event_)
        num_events += 1
        is_done = si.iterate()
        # if is_done:
        #     print('event')
        #     print(event_)

    # offset = stop - event_
    # print(f'offset:, {stop}, {event_}')

    return num_events, event_


def calculate_num_valids(cfg_dict, pair):
    to_ = cfg_dict[pair[0]]
    from_ = cfg_dict[pair[1]]

    # Get the cycle start addr of to as the target
    cyc_start_to = to_['cycle_starting_addr'][0]

    # print('before')

    num_events, last_event = get_num_events(from_cfg=from_, stop=cyc_start_to)

    # print(f"{num_events}, {last_event}")
    # print('got num events')

    cyc_offset = cyc_start_to - last_event

    return num_events, cyc_offset


def process_json(json_path, verbose=False):

    with open(json_path, 'r') as j_file:
        json_data = json.load(j_file)

    # print(json_data)

    all_data = {}
    # for key, value in json_data.items():
    #     print(f"{key}==>{value}")
    #     print(f"{key}")
    #     print(key)

    # print(json_data['top'])
    # print(json_data['namespaces']['global']['modules'])
    modules = json_data['namespaces']['global']['modules']
    # print(json_data['global'])

    all_ubs = []

    for key, value in modules.items():
        if '_ub' in key:
            # print(key)
            all_ubs.append(key)

    # print("ALL UB")
    # print(all_ubs)
    # print(modules['conv_stencil_ub'])
    # ubs = modules['conv_stencil_ub']['instances']

    # ubs = modules['conv_stencil_ub']['instances']

    # print('UB, CTRL, CYCLE_START, BITS')

    normal = ""
    valid_only = ""

    for ubs_ in all_ubs:

        # print(ubs_)
        # ubs = modules[ubs_]['instances']
        if 'instances' not in modules[ubs_]:
            # print(f"CONTINUING ON {ubs_}")
            # print(modules[ubs_])
            continue
        ubs = modules[ubs_]['instances']
        # Loop through ubs, skip clock enables
        for key in ubs.keys():

            if 'clk_en' in key or 'ub' not in key:
                continue

            if key not in all_data:
                all_data[key] = {}
            # print(key)
            # print(value)
            # print(key)
            # VBOSE = False
            # if 'ub_input_cgra_stencil_BANK_7_garnet' in key:
            #     print("FOUND IT")
            #     VBOSE = True

            # ub_config = ubs[key]['modargs']['config']
            ub_config = ubs[key]

            cfg_dict = None

            if isinstance(ub_config, list):
                for item in ub_config:
                    if isinstance(item, dict):
                        cfg_dict = item
                        break
            else:
                cfg_dict = ub_config

            # print(cfg_dict)

            if 'modargs' in cfg_dict:
                # print("TRIGGERED")
                cfg_dict = cfg_dict['modargs']['config']

                if isinstance(cfg_dict, list):
                    for item in cfg_dict:
                        if isinstance(item, dict):
                            cfg_dict = item
                            break
                else:
                    cfg_dict = ub_config

            # print(cfg_dict)

            for cfg_ in allowable_configs:
                if cfg_ in cfg_dict:
                    local_cfg = cfg_dict[cfg_]
                    # print(f"{key}, {cfg_}, {local_cfg['cycle_starting_addr'][0]}, {kratos.clog2(local_cfg['cycle_starting_addr'][0])}")
                    normal += f"{key}, {cfg_}, {local_cfg['cycle_starting_addr'][0]}, {kratos.clog2(local_cfg['cycle_starting_addr'][0])}\n"
                    all_data[key][cfg_] = local_cfg['cycle_starting_addr'][0]

                # print(cfg_)
                # print(local_cfg['cycle_starting_addr']

            for pair_ in pairs:

                to = pair_[0]
                from_l = pair_[1]

                curr_best = 1000000
                valid_only_tmp = ""

                for from_ in from_l:

                    if VBOSE:
                        print(from_)
                    # print(from_)

                # print(pair_)
                    if to in cfg_dict and from_ in cfg_dict:
                        num_valids, cycle_offset = calculate_num_valids(cfg_dict, (to, from_))
                        # if 'sram2tb_1' in to and 'agg2sram_1' in from_:
                        #     print(f"on it: {num_valids}, {cycle_offset}")
                    # to = cfg_dict[pair_[0]]
                    # from = cfg_dict[pair_[1]]
                    # print(f"{key}, {pair_}, {num_valids}, {cycle_offset}, {kratos.clog2(num_valids)}, {kratos.clog2(cycle_offset)}")
                        clog2valids = kratos.clog2(num_valids)
                        clog2offset = kratos.clog2(cycle_offset)
                        if clog2valids == 0:
                            clog2valids = 1
                        if clog2offset == 0:
                            clog2offset = 1
                        if (clog2valids + clog2offset) < curr_best:
                            og = all_data[key][to]
                            og_bits = kratos.clog2(og)
                            if og_bits == 0:
                                og_bits = 1
                            valid_only_tmp = f"{key}, {to}:{from_}, {num_valids}, {cycle_offset}, {clog2valids}, {clog2offset}, {clog2valids + clog2offset}, {og_bits}, {og}\n"
                            curr_best = (clog2valids + clog2offset)
                # print("ADDING")
                # print(valid_only)
                valid_only += valid_only_tmp
                # print(valid_only)
                # print(num_valids)
                # print(cycle_offset)
                # exit()

    if verbose:
        # print('UB, CTRL, CYCLE_START, BITS')
        # print(normal)

        print('UB, PAIR, NUM VALIDS, CYCLE OFFSET, BITS VALIDS, BITS OFFSET, BITS SUM, ORIGINAL BITS, ORIGINAL VALUE')
        print(valid_only)

        # print(u)


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Analyze Schedules')
    parser.add_argument('--json_path', type=str, default=None)
    parser.add_argument('--verbose', action="store_true")

    args = parser.parse_args()

    json_path = args.json_path
    verbose = args.verbose

    process_json(json_path=json_path,
                 verbose=verbose)
