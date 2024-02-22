#!/usr/bin/env python3
import json

f = open("./data.json")

x = f.read()

f.close()

y = json.loads(x)

print (len(y))
