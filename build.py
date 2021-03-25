#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 ninja

import glob
import subprocess
from pathlib import Path

files = glob.glob("/Users/clinton/roam/*.org")

with open('build.ninja', 'w') as ninja_file:
    ninja_file.write("""
rule org2md
  command = emacs --batch -l ~/.emacs.d/init_for_pub.el -l publish.el --eval \"(clinton/publish \\"$in\\")"
  description = org2md $in
""")
    
    for f in files:
        with open(f) as file_text:
            if '#+roam_tags: public' in file_text.read() or '#+ROAM_TAGS: public' in file_text.read():
                path = Path(f)
                output_file = f"/Users/clinton/dev/digital-garden/content/{path.with_suffix('.md').name}"
                ninja_file.write(f"""build {output_file}: org2md {path}\n""")
