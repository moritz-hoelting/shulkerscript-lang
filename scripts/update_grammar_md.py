#!/usr/bin/env python3

import re
import os
from pathlib import Path
from collections import defaultdict, deque

ebnf_blocks = []
rule_defs = {}
rule_deps = defaultdict(set)

ebnf_fence_start = re.compile(r"^\s*///\s*```\s*ebnf\s*$")
ebnf_fence_end = re.compile(r"^\s*///\s*```\s*$")
doc_comment_prefix = re.compile(r"^\s*///\s?(.*)$")

rule_start_pattern = re.compile(r"^\s*([A-Za-z_]\w*)\s*:")
rule_ref_pattern = re.compile(r"\b([A-Za-z_]\w*)\b")


def find_project_root() -> Path | None:
    current = Path.cwd()
    while current != current.parent:
        cargo_toml = current / "Cargo.toml"
        if cargo_toml.exists():
            text = cargo_toml.read_text(encoding="utf-8")
            if re.search(r'(?m)^\s*name\s*=\s*"shulkerscript"\s*$', text):
                return current
        current = current.parent
    return None


root_dir = find_project_root()
if not root_dir:
    raise SystemExit(
        "Could not find Cargo.toml of package 'shulkerscript' in this or any parent directory."
    )

if Path.cwd() != root_dir:
    os.chdir(root_dir)
    print(f"Changed working directory to {root_dir}")

previous_rules = set()

with open("grammar.md", "r", encoding="utf-8") as f:
    rule_header_pattern = re.compile(r"## (\w+)")
    for line in f:
        m = rule_header_pattern.match(line)
        if m:
            previous_rules.add(m.group(1))

for path in Path(".").rglob("*.rs"):
    with path.open(encoding="utf-8") as f:
        in_block = False
        current_block_lines = []

        for line in f:
            if not in_block and ebnf_fence_start.match(line):
                in_block = True
                current_block_lines = []
                continue
            if in_block:
                if ebnf_fence_end.match(line):
                    block_text = "\n".join(current_block_lines)

                    ebnf_blocks.append(block_text)

                    current_rule_name = None
                    current_rule_lines = []
                    for ln in current_block_lines:
                        m = rule_start_pattern.match(ln)
                        if m:
                            if current_rule_name:
                                full_def = "\n".join(current_rule_lines)
                                rule_defs[current_rule_name] = full_def
                                refs = set(rule_ref_pattern.findall(full_def))
                                refs.discard(current_rule_name)
                                rule_deps[current_rule_name].update(refs)
                            current_rule_name = m.group(1)
                            current_rule_lines = [ln]
                        else:
                            if current_rule_name:
                                current_rule_lines.append(ln)

                        if current_rule_name:
                            full_def = "\n".join(current_rule_lines)

                            rule_defs[current_rule_name] = full_def
                            refs = set(rule_ref_pattern.findall(full_def))
                            refs.discard(current_rule_name)
                            rule_deps[current_rule_name].update(refs)

                        in_block = False
                        continue

                m = doc_comment_prefix.match(line)
                if m:
                    current_block_lines.append(m.group(1))

if "Program" not in rule_defs:
    raise SystemExit("Root rule 'Program' not found in EBNF definitions")

visited = set()
order = []
queue = deque(["Program"])

while queue:
    rule = queue.popleft()
    if rule not in visited and rule in rule_defs:
        visited.add(rule)
        order.append(rule)
        for dep in sorted(rule_deps[rule]):
            if dep not in visited:
                queue.append(dep)

unused_rules = sorted(set(rule_defs.keys()) - visited)

if len(unused_rules) > 0:
    print(
        f"Appending {len(unused_rules)} unused rules to the end: {', '.join(unused_rules)}"
    )

order.extend(unused_rules)

with open("grammar.md", "w", encoding="utf-8") as out:
    out.write("# Grammar of the Shulkerscript language\n\n")

    for rule in order:
        out.write(f"## {rule}\n\n```ebnf\n{rule_defs[rule]}\n```\n\n")

print(f"Wrote grammar.md with {len(order)} rules.")
added_rules = set(rule_defs.keys()) - previous_rules
if len(added_rules) > 0:
    print(f"Added rules for: {', '.join(added_rules)}")
removed_rules = previous_rules - set(rule_defs.keys())
if len(removed_rules) > 0:
    print(f"Removed rules for: {', '.join(removed_rules)}")
