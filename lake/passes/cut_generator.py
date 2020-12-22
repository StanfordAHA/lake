import kratos


def cut_generator(gen: kratos.Generator, suffix="_top"):
    # cut the generator from it's parent scope
    parent_gen = gen.internal_generator.parent_generator()
    assert parent_gen is not None, f"{gen.name} does not have a parent"
    new_ports = set()
    for port_name in gen.ports:
        port = gen.ports[port_name]
        # only need to worry about the output
        if port.port_direction == kratos.PortDirection.In.value:
            continue
        new_port = parent_gen.port(port, port_name + suffix)
        # flip the port direction
        new_port.port_direction = kratos.PortDirection.In.value
        new_port.move_sink_to(port, new_port, parent_gen, True)
        new_ports.add(new_port.name)
    # remove from parent
    parent_gen.remove_child_generator(gen.internal_generator)
    # lifting things up
    while parent_gen.parent_generator() is not None:
        p_gen = parent_gen.parent_generator()
        for port_name in new_ports:
            port = parent_gen.get_port(port_name)
            new_port = p_gen.port(port, port_name)
            p_gen.add_stmt(port.assign(new_port))
        parent_gen = p_gen
