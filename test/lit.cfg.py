import lit.formats
from lit.llvm import llvm_config
import os

config.name = 'Watever'
config.test_format = lit.formats.ShTest(True)
config.suffixes = ['.ll', '.c', '.cpp', '.test']

# Add watever to PATH
path = os.path.pathsep.join((config.watever_binary_dir, config.environment['PATH']))
config.environment['PATH'] = path

config.excludes = ['benchmark']
