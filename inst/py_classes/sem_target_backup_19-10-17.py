from pysubgroup.measures import AbstractInterestingnessMeasure, \
    BoundedInterestingnessMeasure

# import numpy as np
# from rpy2.robjects import r, pandas2ri
# from rpy2 import robjects


class SEMTarget(object):
    def __init__(self):
        pass

    def __repr__(self):
        return "T: Structural Equation Model"

    def __eq__(self, other):
        return self.__dict__ == other.__dict__

    def __lt__(self, other):
        return str(self) < str(other)

    def get_attributes(self):
        return []

    def test(self):
        return myval


class TestQF(AbstractInterestingnessMeasure):
    def __init__(self):
    	pass

    def evaluate_from_dataset(self, data, subgroup, weighting_attribute=None):
        if len(subgroup.subgroup_description) == 0:
            return -1
        print(subgroup)
        instances = subgroup.subgroup_description.covers(data)

        if (instances.sum() < 10):
            return -1

        rval = f_fit(instances)

        return rval

    def is_applicable(self, subgroup):
        return isinstance(subgroup.target, SEMTarget)

    def supports_weights(self):
        return False
