import os

from mflowgen.components import Graph, Step

def construct():

        g = Graph()

        # KALHAN
        # Synthesize with this as 0, 1, and 2
        opt_lvl = 0
        iterator_support = 6
        config_width = 16


        # KALHAN USE TSMC16
        adk_name = 'tsmc16'
        adk_view = 'multicorner-multivt'

        #adk_name = 'freepdk-45nm'
        #adk_view = 'view-tiny'

        parameters = {
            'construct_path': __file__,
            'design_name': f"optimization_test_iter_support_{iterator_support}_cfg_width_{config_width}_opt_lvl_{opt_lvl}",
            'clock_period': 1.0,
            'adk': adk_name,
            'adk_view': adk_view
            }

        print(parameters)

        this_dir = os.path.dirname(os.path.abspath(__file__))

        g.set_adk(adk_name)
        adk = g.get_adk_step()

        constraints = Step('constraints', default=True)

        dc = Step("cadence-genus-synthesis", default=True)
        rtl = Step( this_dir + '/rtl')

        # Add steps
        g.add_step(dc)
        g.add_step(rtl)
        g.add_step(constraints)

        # Connect steps
        g.connect_by_name(rtl, dc)
        g.connect_by_name(adk, dc)
        g.connect_by_name(constraints, dc)

        g.param_space(rtl, 'opt_lvl', [0, 1, 2])

        g.update_params(parameters)

        return g

if __name__=='__main__':
    g = construct()

