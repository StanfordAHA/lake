import ast

if __name__ == "__main__":
    # Parse the AST
    dict = "{'clock_period': 1000}"

    out = ast.literal_eval(dict)
    # Print the AST
    print(out)
    # Evaluate the AST