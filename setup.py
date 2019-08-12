from setuptools import setup

setup(
        name='lake',
        packages=[
            "lake"
        ],
        version='0.0.1',
        author='Maxwell Strange',
        author_email='mstrange@stanford.edu',
        description='Memory Generator based on Kratos: The God of War.',
        url="https://github.com/StanfordAHA/lake",
        python_requires=">=3.5",
        install_requires=[
            "kratos"
        ]
)
