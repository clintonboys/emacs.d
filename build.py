#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 ninja
import re
import glob
import subprocess
from pathlib import Path

files = glob.glob("/Users/clinton/roam/*.org")

DYNAMIC_FILES = {'/Users/clinton/roam/20210316202727-recent-updates.org': '/Users/clinton/dev/digital-garden/content/20210316202727-recent-updates.md'}
i = 0

with open('build.ninja', 'w') as ninja_file:
    ninja_file.write("""
rule org2md
  command = emacs --batch -l ~/.emacs.d/init_for_pub.el -l publish.el --eval \"(clinton/publish \\"$in\\")"
  description = org2md $in
rule touch
  command = touch $out
""")

    ninja_file.write("""
rule orgbabelrun
  command = bash execute_org.sh $in
  description = orgbabelrun $in
""")
    for dynamic_file_source, dynamic_file_target in DYNAMIC_FILES.items():
        ninja_file.write(f"""
build f_{i}.dummy: touch
build dummy_{dynamic_file_target}: phony f_{i}.dummy
build dummy_{dynamic_file_source}: orgbabelrun {dynamic_file_source}\n""")
        i += 1
    for f in files:
        with open(f) as file_text:
            if ('#+roam_tags: public' in file_text.read().lower()):
                path = Path(f)
                output_file = f"/Users/clinton/dev/digital-garden/content/{path.with_suffix('.md').name}"
                ninja_file.write(f"""build {output_file}: org2md {path}\n""")
                pattern = re.compile(r'\[\[file\:(\d{14}[^\[\]]*)\]\[.*\]\]')
                # text = open(f).read()
                # for backlink in re.findall(pattern, text):
                #     ninja_file.write(f"""build {output_file}: org2md {path}\n""")
                #     print(path, backlink)

