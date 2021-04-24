#!/usr/bin/env python3

import json
import pathlib

def fix(compdb):
    for compile in filter(lambda c: c["arguments"][0].endswith("ccache"), compdb):
        del compile["arguments"][0]

    for compile in filter(lambda c: c["file"].endswith(".cpp"), compdb):
        dir = compile["directory"]
        args = compile["arguments"]
        compile["file"] = str(pathlib.Path(dir, compile["file"]).resolve())
        for i in range(1, len(args)):
            if args[i].startswith("-I"):
                args[i] = "-I" + str(pathlib.Path(dir, args[i][2:]).resolve())
            elif args[i - 1] == "-o" or args[i].endswith(".cpp"):
                args[i] = str(pathlib.Path(dir, args[i]).resolve())
    return compdb

if __name__ == "__main__":
    with open("compile_commands.json") as f:
        compdb = fix(json.load(f))
    json.dump(compdb, open("compile_commands.json", "w"), indent=2)
