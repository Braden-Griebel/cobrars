import pytest
import pycobrars


def test_sum_as_string():
    assert pycobrars.sum_as_string(1, 1) == "2"
