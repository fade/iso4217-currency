This system gathers a list of iso4217 currency codes from the wikipedia website, and returns them as a list of currency objects which contain slots for the the iso currency code, the description, as well, as pointers to the iso codes of the currency that replaces the object in the event that it has been superseded. Forex, the irish punt is replaced by the euro.

ex: 

ISO4217-CODES> (gather-all-monies (get-currency-codes))
=> (#<ISO-CURRENCY-CODE #x302004409A7D> #<ISO-CURRENCY-CODE #x302004409A1D> #<ISO-CURRENCY-CODE #x3020044099BD> #<ISO-CURRENCY-CODE #x30200440995D> #<ISO-CURRENCY-CODE #x3020044098FD> #<ISO-CURRENCY-CODE #x30200440989D> #<ISO-CURRENCY-CODE #x30200440983D> #<ISO-CURRENCY-CODE #x3020044097DD> #<ISO-CURRENCY-CODE #x30200440977D> #<ISO-CURRENCY-CODE #x30200440971D> #<ISO-CURRENCY-CODE #x3020044096BD> #<ISO-CURRENCY-CODE #x30200440A14D> 
...
#<ISO-CURRENCY-CODE #x302004431DED> #<ISO-CURRENCY-CODE #x302004431D8D> #<ISO-CURRENCY-CODE #x302004431D2D> #<ISO-CURRENCY-CODE #x302004431CCD> #<ISO-CURRENCY-CODE #x302004431C6D> #<ISO-CURRENCY-CODE #x302004431C0D> #<ISO-CURRENCY-CODE #x302004431BAD> #<ISO-CURRENCY-CODE #x302004431B4D>)
ISO4217-CODES>[]


