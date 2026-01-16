import os
import lit.llvm

parent_config = os.path.join(os.path.dirname(__file__), "..", "lit.cfg.py")
lit_config.load_config(config, parent_config)

config.name = "Watever Benchmarks"
