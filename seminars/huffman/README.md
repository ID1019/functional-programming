## Getting started

In this seminar session we will look at different ways to represent
data, using lists, trees and tuples to find the best representation. The
*best representation* could of course mean many things, we might need a
representation that gives us efficient code or we might want a
representation that is easy to explain, implement and maintain. We will
start by using quite simple representations and then refine them to gain
better performance.

To have something to work with we will implement the *Huffman* encoding
and decoding functions. You should do some reading on Huffman coding,
this text will not explain the algorithm but how to implement it.

## Huffman overview

Huffman coding can divided into two parts, one part is how to construct
the coding tables and the other, much simpler, is how to encode or
decode a text using the tables.

The idea behind Huffman coding is of course to encode frequent
characters with few bits and infrequent characters with more bits. To
keep things simple we will represent sequences of bits as lists of zeros
and ones but this could of course be changes if we intend to do a real
implementation that reads and writes to files. For our experiments it is
sufficient.

The table should give a one to one maping from characters to codes but
we might use one representation when we encode text and another when we
decode text; the information it holds is the same but we might want to
do this for efficiency.

Once we are done we will have a module that defines the following
functions:
  - tree(sample): create a *Huffman tree* given a sample text.
  - encode\_table(tree): create an *encoding table* containing the
    mapping from characters to codes given a Huffman tree.
  - decode\_table(tree): create an *decoding table* containing the
    mapping from codes to characters given a Huffman tree.
  - encode(text, table): encode the text using the mapping in the table,
    return a sequence of bits.
  - decode(sequence, table): decode the bit sequence using the mapping
    in table, return a text.
    
Start by defining the module, some compile directives, things that
are good to have and dummy code for the functions.
```elixir
defmodule Huffman do

  def sample do
    'the quick brown fox jumps over the lazy dog
    this is a sample text that we will use when we build
    up a table we will only handle lower case letters and
    no punctuation symbols the frequency will of course not
    represent english but it is probably not that far off'
  end

  def text, do: 'this is something that we should encode'
  
  def test do
    sample = sample()
    tree = tree(sample)
    encode = encode_table(tree)
    decode = decode_table(tree)
    text = text()
    seq = encode(text, encode)
    decode(seq, decode)
  end
  
  def tree(sample) do
    # To implement...
  end
  
  def encode_table(tree) do
    # To implement...
  end
  
  def decode_table(tree) do
    # To implement...
  end
  
  def encode(text, table) do
    # To implement...
  end
  
  def decode(seq, tree) do
    # To implement...
  end
end
```

## The table

In order to create the Huffman tree we need first to find out the frequency
distribution in our sample text. Once we have the frequency
distribution we can start building the tree.

```elixir
def tree(sample) do
  freq = freq(sample)
  huffman(freq)
end
```

The sample is of course a list of characters (`[102,111,111]`), you should run through
this list and collect the frequencies of the characters. If "foo"
was the sample text we should have the frequencies _f/1_, _o/2_. 
How would you represent this information? Note that you need
not know beforehand which characters that will occur in the sample.

You will probably end up with a structure that looks like this, but
how you represent the frequencies is up to you.
