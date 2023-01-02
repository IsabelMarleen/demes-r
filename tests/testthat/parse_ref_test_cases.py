from ruamel.yaml import YAML
import json
import os

yaml=YAML()

with open('tests/testthat/py_json_temp/valid/structure_01.json', 'r') as file:
    configuration = json.load(file)
    
with open('tests/testthat/py_preparsed/valid/structure_01.yaml', 'w') as yaml_file:
    yaml.dump(configuration, yaml_file)
