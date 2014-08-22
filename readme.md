###csvp

A command line dsl for transforming csv files.

The structure of a ```csvp``` command is typically one or more group commands followed by one or
more summary commands.

Group commands are simply ```group n``` where n is a column number.

Summary commands are made up of a summary type and a column number.

__Current summary types__

<table>
  <tr><td>sum</td></tr>
  <tr><td>max</td></tr>
  <tr><td>min</td></tr>
  <tr><td>stddev</td></tr>
  <tr><td>avg</td></tr>
  <tr><td>count</td></tr>
</table>

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

``` <test.csv | csvp 'group 0; group 1; avg 2; stddev 2; max 2; sum 2;'   ```

<pre>
+-------------------------------------+----------+-------+ 
| bob                                 | deposit  | 10.0  |
| bob                                 | deposit  | 5.0   |
| = 7.5 2.5 10.0 15.0                 |          |       |
|                                     |          |       |
| bob                                 | withdraw | 15.0  |
| = 15.0 0.0 15.0 15.0                |          |       |
|                                     |          |       |
| jim                                 | deposit  | 100.0 |
| = 100.0 0.0 100.0 100.0             |          |       |
|                                     |          |       |
| jim                                 | withdraw | 20.0  |
| jim                                 | withdraw | 5.0   |
| jim                                 | withdraw | 50.0  |
| = 25.0 18.708286933869708 50.0 75.0 |          |       |
|                                     |          |       |
+-------------------------------------+----------+-------+
</pre>


###JSON

You can now also supply a `--json` switch:

``` <test.csv | csvp 'group 0; avg 2; count 2;' --json ```

```json
[
  [
    {
      "rows": [
        [
          "bob",
          "deposit",
          "10.0"
        ],
        [
          "bob",
          "withdraw",
          "15.0"
        ],
        [
          "bob",
          "deposit",
          "5.0"
        ]
      ],
      "summary": [
        "10.0",
        "3.0"
      ]
    },
    ...
```

