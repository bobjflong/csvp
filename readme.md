###csvp

A command line dsl for transforming csv files.

Currently csvp supports the ``` group ``` and ``` avg ``` commands. ```stddev``` coming soon.

###Simple Example

Given the following csv:

<pre>
bob,deposit,10
jim,withdraw,20
bob,withdraw,15
jim,withdraw,5
jim,withdraw,50
bob,deposit,5
jim,deposit,100
</pre>

To group by name, then subgroup by transaction type, then summarize by value, you'd simply run:

``` cat test.csv | ./CSVPivot 'group 0; group 1; avg 2 ;' ```

<pre>
bob,deposit,10.0
bob,deposit,5.0

7.5

bob,withdraw,15.0

15.0

jim,deposit,100.0

100.0

jim,withdraw,20.0
jim,withdraw,5.0
jim,withdraw,50.0

25.0
</pre>

