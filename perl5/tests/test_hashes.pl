#!/usr/bin/env perl
# Test: Hash operations

my %person = (
    name => "Alice",
    age => 30,
    city => "New York"
);

print "Name: " . $person{name} . "\n";
print "Age: " . $person{age} . "\n";

my @keys = keys(%person);
my @values = values(%person);

foreach my $key (@keys) {
    print "$key => $person{$key}\n";
}
