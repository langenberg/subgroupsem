from pysubgroup.measures import AbstractInterestingnessMeasure, \
    BoundedInterestingnessMeasure

import numpy as np
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

#    def test(self):
#        return myval

    def calculate_statistics(self, subgroup, data, weighting_attribute=None):
        if weighting_attribute is not None:
            raise NotImplemented("Attribute weights with SEM targets are not yet implemented.")
        sg_instances = subgroup.subgroup_description.covers(data)
        subgroup.statistics['size_sg'] = np.sum(sg_instances)

class GeneralizationAwareQF(AbstractInterestingnessMeasure):
    def __init__(self, qf):
        self.qf = qf
        self.cache = {}

    def evaluate_from_dataset(self, data, subgroup, weighting_attribute=None):
        q_subgroup = self.qf.evaluate_from_dataset (data, subgroup, weighting_attribute)

        # compute quality of all generalizations
        selectors = subgroup.subgroup_description.selectors
        generalizations = ps.powerset(selectors)
        max_q = 0
        for sels in generalizations:
            sgd = ps.SubgroupDescription(list(sels))
            if frozenset(sgd.selectors) in self.cache:
                q_sg = self.cache[frozenset(sgd.selectors)]
            else:
                sg = ps.Subgroup(subgroup.target, sgd)
                q_sg = self.qf.evaluate_from_dataset (data, sg, weighting_attribute)
                self.cache[frozenset(sgd.selectors)] = q_sg
            max_q = max(max_q, q_sg)
        return q_subgroup - max_q

    def is_applicable(self, subgroup):
        return self.qf.is_applicable(subgroup)

    def supports_weights(self):
        return self.qf.supports_weights()

class TestQF(AbstractInterestingnessMeasure):
    def __init__(self):
    	pass

    def evaluate_from_dataset(self, data, subgroup, weighting_attribute=None):
        if len(subgroup.subgroup_description) == 0:
            return -1
#        print(subgroup)
        instances = subgroup.subgroup_description.covers(data)
        variables = [str(selector.attribute_name) for selector in subgroup.subgroup_description.selectors]

        if (instances.sum() < 10):
            return -1

        rval = f_fit(instances, variables)

        return rval

    def is_applicable(self, subgroup):
        return isinstance(subgroup.target, SEMTarget)

    def supports_weights(self):
        return False
