from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name='lake-aha',
    version="0.0.3",
    author='Maxwell Strange',
    author_email='mstrange@stanford.edu',
    description='Memory Generator based on Kratos: The God of War.',
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/StanfordAHA/lake",
    python_requires=">=3.5",
    packages=find_packages(exclude=("tests",)),
    install_requires=[
        "kratos",
        "fault",
        "magma-lang",
        "pytest"
    ]
)
