from setuptools import setup, find_packages

setup(
    name = 'drawille',
    version = '0.0.4',
    author = 'Adam Tauber',
    author_email = 'asciimoo@gmail.com',
    description = ('Drawing in terminal with unicode braille characters'),
    license = 'AGPLv3+',
    keywords = "terminal braille drawing canvas console",
    url = 'https://github.com/asciimoo/drawille',
    scripts = [],
    py_modules = ['drawille'],
    packages = find_packages(),
    install_requires = [],
    download_url = 'https://github.com/asciimoo/drawille/tarball/master',
    # TODO
    #entry_points={
    #    "console_scripts": ["drawille=drawille:__main__"]
    #},
    classifiers = [
        "Development Status :: 4 - Beta",
        "Topic :: Utilities",
        'Environment :: Console',
        'License :: OSI Approved :: GNU Affero General Public License v3',
        'Intended Audience :: Developers',
        'Natural Language :: English',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.3',
    ],
)
