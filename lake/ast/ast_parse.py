import ast
from pprint import pprint

class Analyzer(ast.NodeVisitor):
    def __init__(self):
        self.stats = {"import": [], "from": [], "assign": []}

    def visit_Import(self, node):
        for alias in node.names:
            self.stats["import"].append(alias.name)
        self.generic_visit(node)

    def visit_ImportFrom(self, node):
        for alias in node.names:
            self.stats["from"].append(alias.name)
        self.generic_visit(node)

    def visit_Assign(self, node: ast.Assign):
        # for alias in node.names:
            # self.stats["assign"].append(alias.name)
        print(f"assign statement has {len(node.targets)} targets")
        print(node.targets[0].id)
        print(node.value.left.left.left.id)
        self.generic_visit(node)

    def report(self):
        pprint(self.stats)


if __name__ == "__main__":
    with open("ast_test.py", "r") as source:
        tree = ast.parse(source.read())

    analyzer = Analyzer()
    analyzer.visit(tree)
    analyzer.report()
