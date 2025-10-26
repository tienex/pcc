#!/usr/bin/env perl
# Test: Array operations

my @numbers = (1, 2, 3, 4, 5);

push(@numbers, 6);
my $last = pop(@numbers);
my $first = shift(@numbers);
unshift(@numbers, 0);

print "Array length: " . length(@numbers) . "\n";

foreach my $num (@numbers) {
    print "$num ";
}
print "\n";
