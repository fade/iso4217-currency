This system gathers a list of iso4217 currency codes from the
wikipedia website, and returns them as a list of currency objects
which contain slots for the the iso currency code, the description, as
well as pointers to the iso codes of the currency that replaces the
object in the event that it has been superseded. Forex, the irish punt
is replaced by the euro.

ex: 

```common-lisp

    ISO4217-CODES> (gather-all-monies (get-currency-codes))
    =>
    (list of currency objects in CL printable form that the github
    markdown dynamo refuses to display properly because the representation
    contains angle brackets)

    ISO4217-CODES> (describe (first *)) 
    =>
    \<ISO-CURRENCY-CODE {1005CA6563}>
      [standard-object]
    
    Slots with :INSTANCE allocation:
      ISO-CODE                       = "ZWL"
      HISTORIC                       = T
      DESCRIPTION                    = "Zimbabwean dollar"
      FROM                           = "2009-02-03"
      UNTIL                          = "2009-04-12"
      USED-BY                        = NIL
      REPLACED-BY                    = "USD/Zimbabwean bonds"
    ; No value
    ISO4217-CODES> 

    ISO4217-CODES> (write-iso-file #P "/tmp/currency-codes")
    =>
    HISTORICAL! "ZWL" "932" "2" "Zimbabwean dollar" "2009-02-03" "2009-04-12" "USD/Zimbabwean bonds"
    HISTORICAL! "ZWR" "935" "2" "Zimbabwean dollar" "2008-08-01" "2009-02-02" "ZWL (USD/Zimbabwean     bonds)"
    HISTORICAL! "ZWN" "942" "2" "Zimbabwean dollar" "2006-08-01" "2008-07-31" "ZWR (USD/Zimbabwean     bonds)"
    HISTORICAL! "ZWD" "716" "2" "Zimbabwean dollar" "1980-04-18" "2006-07-31" "ZWN (USD/Zimbabwean     bonds)"
    HISTORICAL! "ZWC" "..." "2" "Rhodesian dollar" "1970-02-17" "1980" "ZWD (USD/Zimbabwean bonds)"
    HISTORICAL! "ZRN" "180" "2" "Zairean new zaire" "1993" "1997" "CDF"
    HISTORICAL! "ZRZ" "180" "3" "Zairean zaire" "1967" "1993" "ZRN (CDF)"
    
    ...
    
    STANDARD! "AOA" "973" "2" "Angolan kwanza" "Angola"
    STANDARD! "ANG" "532" "2" "Netherlands Antillean guilder" "Curaçao (CW), Sint Maarten (SX)"
    STANDARD! "AMD" "051" "2" "Armenian dram" "Armenia"
    STANDARD! "ALL" "008" "2" "Albanian lek" "Albania"
    STANDARD! "AFN" "971" "2" "Afghan afghani" "Afghanistan"
    STANDARD! "AED" "784" "2" "United Arab Emirates dirham" "United Arab Emirates"
    NIL
    ISO4217-CODES> 

```

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


