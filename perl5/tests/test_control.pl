#!/usr/bin/env perl
# Test: Control structures

my $x = 10;

if ($x > 5) {
    print "x is greater than 5\n";
} elsif ($x == 5) {
    print "x equals 5\n";
} else {
    print "x is less than 5\n";
}

while ($x > 0) {
    print "$x\n";
    $x--;
}

for (my $i = 0; $i < 10; $i++) {
    print "i = $i\n";
}

my @items = (1, 2, 3, 4, 5);
foreach my $item (@items) {
    print "Item: $item\n";
}
