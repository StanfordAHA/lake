"""Unit tests for Spec.extract_compiler_information and its helpers."""
import pytest
from lake.spec.spec import Spec
from lake.spec.port import Port
from lake.utils.spec_enum import Runtime, Direction, MemoryPortType
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ScheduleGenerator
from lake.spec.storage import SingleBankStorage
from lake.spec.memory_port import MemoryPort


def _build_simple_dual_port(storage_capacity=1024, data_width=16, dims=6):
    """Build a simple dual-port Spec (separate R and W MemoryPorts)."""
    ls = Spec()
    in_port = Port(ext_data_width=data_width, int_data_width=data_width,
                   runtime=Runtime.STATIC, direction=Direction.IN)
    out_port = Port(ext_data_width=data_width, int_data_width=data_width,
                    runtime=Runtime.STATIC, direction=Direction.OUT)
    ls.register(in_port, out_port)

    in_id = IterationDomain(dimensionality=dims, extent_width=16)
    in_ag = AddressGenerator(dimensionality=dims)
    in_sg = ScheduleGenerator(dimensionality=dims)
    ls.register(in_id, in_ag, in_sg)

    out_id = IterationDomain(dimensionality=dims, extent_width=16)
    out_ag = AddressGenerator(dimensionality=dims)
    out_sg = ScheduleGenerator(dimensionality=dims)
    ls.register(out_id, out_ag, out_sg)

    stg = SingleBankStorage(capacity=storage_capacity)
    wr_mp = MemoryPort(data_width=data_width, mptype=MemoryPortType.W, delay=0)
    rd_mp = MemoryPort(data_width=data_width, mptype=MemoryPortType.R, delay=1)
    ls.register(stg, wr_mp, rd_mp)

    ls.connect(in_port, in_id)
    ls.connect(in_port, in_ag)
    ls.connect(in_port, in_sg)
    ls.connect(out_port, out_id)
    ls.connect(out_port, out_ag)
    ls.connect(out_port, out_sg)
    ls.connect(in_port, wr_mp)
    ls.connect(out_port, rd_mp)
    ls.connect(wr_mp, stg)
    ls.connect(rd_mp, stg)

    return ls, stg


def _build_single_port_wide_fetch(storage_capacity=1024, data_width=16,
                                  dims=6, vec_width=4):
    """Build a single-port wide-fetch Spec (shared RW MemoryPort)."""
    ls = Spec()
    in_port = Port(ext_data_width=data_width,
                   int_data_width=data_width * vec_width,
                   vec_capacity=2,
                   runtime=Runtime.STATIC, direction=Direction.IN)
    out_port = Port(ext_data_width=data_width,
                    int_data_width=data_width * vec_width,
                    vec_capacity=2,
                    runtime=Runtime.STATIC, direction=Direction.OUT)
    ls.register(in_port, out_port)

    in_id = IterationDomain(dimensionality=dims, extent_width=16)
    in_ag = AddressGenerator(dimensionality=dims)
    in_sg = ScheduleGenerator(dimensionality=dims)
    ls.register(in_id, in_ag, in_sg)

    out_id = IterationDomain(dimensionality=dims, extent_width=16)
    out_ag = AddressGenerator(dimensionality=dims)
    out_sg = ScheduleGenerator(dimensionality=dims)
    ls.register(out_id, out_ag, out_sg)

    stg = SingleBankStorage(capacity=storage_capacity)
    shared_rw_mp = MemoryPort(data_width=data_width * vec_width,
                              mptype=MemoryPortType.RW, delay=1)
    ls.register(stg, shared_rw_mp)

    ls.connect(in_port, in_id)
    ls.connect(in_port, in_ag)
    ls.connect(in_port, in_sg)
    ls.connect(out_port, out_id)
    ls.connect(out_port, out_ag)
    ls.connect(out_port, out_sg)
    ls.connect(in_port, shared_rw_mp)
    ls.connect(out_port, shared_rw_mp)
    ls.connect(shared_rw_mp, stg)

    return ls, stg


# ---------------------------------------------------------------------------
# Tests for extract_compiler_information (full output)
# ---------------------------------------------------------------------------

class TestExtractCompilerInformationDualPort:
    """Test collateral extraction on a simple dual-port Spec."""

    @pytest.fixture(autouse=True)
    def setup(self):
        self.ls, self.stg = _build_simple_dual_port()
        self.collateral = self.ls.extract_compiler_information(
            controller_name_map={self.stg: 'mem'})

    def test_controller_name(self):
        assert self.collateral['controller_name'] == ['mem']

    def test_capacity(self):
        assert self.collateral['capacity'] == {'mem': 1024}

    def test_word_width(self):
        assert self.collateral['word_width'] == {'mem': 1}

    def test_in_port_width(self):
        assert self.collateral['in_port_width'] == {'mem': 1}

    def test_out_port_width(self):
        assert self.collateral['out_port_width'] == {'mem': 1}

    def test_bank_num(self):
        assert self.collateral['bank_num'] == {'mem': 2}

    def test_single_port_false(self):
        assert self.collateral['single_port'] == {'mem': False}

    def test_dual_port_sram(self):
        assert self.collateral['dual_port_sram'] is True

    def test_fetch_width(self):
        assert self.collateral['fetch_width'] == 1

    def test_iteration_level(self):
        assert self.collateral['iteration_level'] == 6

    def test_iter_level_map(self):
        assert self.collateral['iter_level_map'] == {
            'in2mem_0': 6,
            'mem2out_0': 6,
        }

    def test_load_latency(self):
        assert self.collateral['load_latency'] == 1

    def test_store_latency(self):
        assert self.collateral['store_latency'] == 0

    def test_counter_ub(self):
        assert self.collateral['counter_ub'] == 65535

    def test_multi_sram_accessor_false(self):
        assert self.collateral['multi_sram_accessor'] is False

    def test_interconnect_ports(self):
        assert self.collateral['interconnect_in_num'] == 1
        assert self.collateral['interconnect_out_num'] == 1

    def test_defaults(self):
        assert self.collateral['max_chaining'] == 4
        assert self.collateral['wire_chain_en'] is False


class TestExtractCompilerInformationWideFetch:
    """Test collateral extraction on a single-port wide-fetch Spec."""

    @pytest.fixture(autouse=True)
    def setup(self):
        self.ls, self.stg = _build_single_port_wide_fetch(
            storage_capacity=1024, vec_width=4)
        self.collateral = self.ls.extract_compiler_information(
            controller_name_map={self.stg: 'sram'})

    def test_controller_name(self):
        assert self.collateral['controller_name'] == ['sram']

    def test_capacity(self):
        assert self.collateral['capacity'] == {'sram': 1024}

    def test_word_width(self):
        assert self.collateral['word_width'] == {'sram': 4}

    def test_in_out_port_width(self):
        assert self.collateral['in_port_width'] == {'sram': 4}
        assert self.collateral['out_port_width'] == {'sram': 4}

    def test_bank_num(self):
        assert self.collateral['bank_num'] == {'sram': 1}

    def test_single_port_true(self):
        assert self.collateral['single_port'] == {'sram': True}

    def test_dual_port_sram_false(self):
        assert self.collateral['dual_port_sram'] is False

    def test_fetch_width(self):
        assert self.collateral['fetch_width'] == 4

    def test_multi_sram_accessor_true(self):
        assert self.collateral['multi_sram_accessor'] is True

    def test_load_latency(self):
        assert self.collateral['load_latency'] == 1

    def test_store_latency(self):
        assert self.collateral['store_latency'] == 0

    def test_iteration_level(self):
        assert self.collateral['iteration_level'] == 6

    def test_iter_level_map(self):
        assert self.collateral['iter_level_map'] == {
            'in2sram_0': 6,
            'sram2out_0': 6,
        }


# ---------------------------------------------------------------------------
# Tests for individual helper methods
# ---------------------------------------------------------------------------

class TestResolveControllerNameMap:
    """Test _resolve_controller_name_map."""

    def test_auto_names(self):
        ls, stg = _build_simple_dual_port()
        name_map = ls._resolve_controller_name_map()
        assert stg in name_map
        assert isinstance(name_map[stg], str)

    def test_explicit_map(self):
        ls, stg = _build_simple_dual_port()
        name_map = ls._resolve_controller_name_map({stg: 'custom'})
        assert name_map[stg] == 'custom'


class TestExtractCapacity:
    def test_capacity_value(self):
        ls, stg = _build_simple_dual_port(storage_capacity=2048)
        name_map = {stg: 'mem'}
        assert ls._extract_capacity(name_map) == {'mem': 2048}


class TestExtractMemoryPortInfo:
    def test_dual_port_info(self):
        ls, stg = _build_simple_dual_port()
        info = ls._extract_memory_port_info({stg: 'mem'})
        assert info['dual_port_sram'] is True
        assert info['single_port'] == {'mem': False}
        assert info['bank_num'] == {'mem': 2}

    def test_single_port_rw_info(self):
        ls, stg = _build_single_port_wide_fetch()
        info = ls._extract_memory_port_info({stg: 'sram'})
        assert info['dual_port_sram'] is False
        assert info['single_port'] == {'sram': True}
        assert info['bank_num'] == {'sram': 1}


class TestExtractWordWidth:
    def test_symmetric_widths(self):
        ls, _ = _build_simple_dual_port()
        result = ls._extract_word_width({'mem'}, {'mem': 4}, {'mem': 4})
        assert result == {'mem': 4}

    def test_asymmetric_widths(self):
        ls, _ = _build_simple_dual_port()
        result = ls._extract_word_width({'mem'}, {'mem': 1}, {'mem': 4})
        assert result == {'mem': 1}

    def test_gcd_widths(self):
        ls, _ = _build_simple_dual_port()
        result = ls._extract_word_width({'mem'}, {'mem': 6}, {'mem': 4})
        assert result == {'mem': 2}


class TestExtractMultiSramAccessor:
    def test_shared_mp_true(self):
        ls, _ = _build_single_port_wide_fetch()
        assert ls._extract_multi_sram_accessor() is True

    def test_separate_mp_false(self):
        ls, _ = _build_simple_dual_port()
        assert ls._extract_multi_sram_accessor() is False


class TestExtractIterationInfo:
    def test_dims_and_counter(self):
        ls, stg = _build_simple_dual_port(dims=4)
        # Use dims=4 to verify iteration_level reflects it
        # Need to rebuild with custom dims
        ls2, stg2 = _build_simple_dual_port(dims=4)
        info = ls2._extract_iteration_info({stg2: 'rf'})
        assert info['iteration_level'] == 4
        assert info['counter_ub'] == 65535  # extent_width=16 => 2^16-1
        assert 'in2rf_0' in info['iter_level_map']
        assert 'rf2out_0' in info['iter_level_map']
        assert info['iter_level_map']['in2rf_0'] == 4
        assert info['iter_level_map']['rf2out_0'] == 4


class TestExtractStorageTopology:
    def test_single_storage_no_wiring(self):
        ls, stg = _build_simple_dual_port()
        read_port, write_port = ls._extract_storage_topology({stg: 'mem'})
        assert read_port == {}
        assert write_port == {}


# ---------------------------------------------------------------------------
# Test that the output is JSON-serializable
# ---------------------------------------------------------------------------
class TestJsonSerializable:
    def test_dual_port_json(self):
        import json
        ls, stg = _build_simple_dual_port()
        collateral = ls.extract_compiler_information(
            controller_name_map={stg: 'mem'})
        serialized = json.dumps(collateral)
        assert isinstance(serialized, str)

    def test_wide_fetch_json(self):
        import json
        ls, stg = _build_single_port_wide_fetch()
        collateral = ls.extract_compiler_information(
            controller_name_map={stg: 'sram'})
        serialized = json.dumps(collateral)
        assert isinstance(serialized, str)


# ---------------------------------------------------------------------------
# Test all expected LakeCollateral keys are present
# ---------------------------------------------------------------------------
LAKE_COLLATERAL_KEYS = {
    'controller_name', 'capacity', 'word_width', 'in_port_width',
    'out_port_width', 'bank_num', 'single_port', 'fetch_width',
    'max_chaining', 'iteration_level', 'iter_level_map',
    'load_latency', 'store_latency', 'counter_ub',
    'multi_sram_accessor', 'dual_port_sram', 'wire_chain_en',
    'interconnect_in_num', 'interconnect_out_num',
    'read_port', 'write_port',
}


class TestCollateralKeys:
    def test_dual_port_keys(self):
        ls, stg = _build_simple_dual_port()
        collateral = ls.extract_compiler_information(
            controller_name_map={stg: 'mem'})
        assert set(collateral.keys()) == LAKE_COLLATERAL_KEYS

    def test_wide_fetch_keys(self):
        ls, stg = _build_single_port_wide_fetch()
        collateral = ls.extract_compiler_information(
            controller_name_map={stg: 'sram'})
        assert set(collateral.keys()) == LAKE_COLLATERAL_KEYS
