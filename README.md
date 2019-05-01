This system gathers a list of iso4217 currency codes from the
wikipedia website, and returns them as a list of currency objects
which contain slots for the the iso currency code, the description, as
well as pointers to the iso codes of the currency that replaces the
object in the event that it has been superseded. Forex, the irish punt
is replaced by the euro.

ex: 

<pre>
ISO4217-CODES> (gather-all-monies (get-currency-codes))
=>
(list of currency objects in CL printable form that the github
markdown dynamo refuses to display properly because the representation
contains angle brackets)

ISO4217-CODES>(describe (car *))
=>
#<ISO-CURRENCY-CODE #x302004A1375D>
Class: #<STANDARD-CLASS ISO-CURRENCY-CODE>
Wrapper: #<CCL::CLASS-WRAPPER ISO-CURRENCY-CODE #x30200209770D>
Instance slots
ISO-CODE: "ZWR"
DESCRIPTION: "Zimbabwean dollar A/09"
USED-BY: NIL
REPLACED-BY: "2008-08-01"
</pre>
<pre>
ISO4217-CODES> (write-iso-file #P "/tmp/currency-codes")
=>
[#1][length: 7]  "ZWR"  "935"  "2"  "Zimbabwean dollar A/09"  "2008-08-01"  "2009-02-02"  "ZWL" 
[#2][length: 7]  "ZWN"  "942"  "2"  "Zimbabwean dollar A/08"  "2006-08-01"  "2008-07-31"  "ZWR" 
[#3][length: 7]  "ZWD"  "716"  "2"  "Zimbabwean dollar A/06"  "1980-04-18"  "2006-07-31"  "ZWN" 
[#4][length: 7]  "ZWC"  "..."  "2"  "Rhodesian dollar"  "1970-02-17"  "1980"  "ZWD" 
[#5][length: 7]  "ZRZ"  "..."  "3"  "Zaïrean zaïre"  "1967"  "1993"  "ZRN" 
...
[#278][length: 5]  "AOA"  "973"  "2"  "Angolan kwanza"  "Angola" 
[#279][length: 5]  "ANG"  "532"  "2"  "Netherlands Antillean guilder"  "Curaçao, Sint Maarten" 
[#280][length: 5]  "AMD"  "051"  "2"  "Armenian dram"  "Armenia" 
[#281][length: 5]  "ALL"  "008"  "2"  "Albanian lek"  "Albania" 
[#282][length: 5]  "AFN"  "971"  "2"  "Afghan afghani"  "Afghanistan" 
[#283][length: 5]  "AED"  "784"  "2"  "United Arab Emirates dirham"  "United Arab Emirates" 
NIL
ISO4217-CODES> 
</pre>

and the contents of /tmp/currency-codes:
<pre>
zrz:Zaïrean zaïre
zrn:Zaïrean new zaïre
zmk:Zambian kwacha
zal:South African financial rand (funds code) (discontinued)
yum:Yugoslav dinar
yug:Yugoslav dinar
...
</pre>

etc.


