#!/usr/bin/env python3
import json

#get initial file data
f = open("./data.json")
x = f.read()
f.close()

#load into a dictionary
d = json.loads(x)

#get the dictionary within the list
outerDict = d['d']['results'][0]

#print purchaseorder from dictionary Y
#and print the dictionary and list from the 
#purchaseorderitem dictionary from within dictionary y
print ("PurchaseOrder from dictionary") 
print (outerDict['PurchaseOrder'])
print ("PurchaseOrderItem from dictionary")
print (outerDict['to_PurchaseOrderItem'])

#get purchaseorderitem dictionary from the 
#list in the purchaseorder dictionary
innerDict = outerDict['to_PurchaseOrderItem']['results'][0]
print ("PurchaseOrderItem from newly created dictionary")
print (innerDict)

#get purchaseorderitem dictionary from
#the full JSON object path
innerDic2 = d['d']['results'][0]['to_PurchaseOrderItem']['results'][0]
print ("PurchaseOrderItem from full JSON path")
print (innerDic2)
