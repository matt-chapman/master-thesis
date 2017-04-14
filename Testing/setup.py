from distutils.core import setup
import setuptools

with open('requirements.txt') as f:
    requirements = f.read().splitlines()

setup(
    name='ChangeDetectionExperiments',
    version='',
    install_requires=requirements,
    packages=['ChangeDetection'],
    package_dir={'': 'src'},
    url='',
    license='',
    author='Matt Chapman',
    author_email='matthew.chapman@student.uva.nl',
    description=''
)
