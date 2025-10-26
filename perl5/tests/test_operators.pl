#!/usr/bin/env perl
# Test: Various operators

my $a = 10;
my $b = 20;

# Arithmetic
my $sum = $a + $b;
my $diff = $a - $b;
my $prod = $a * $b;
my $quot = $b / $a;
my $mod = $b % $a;
my $pow = $a ** 2;

# Comparison
if ($a == 10) { print "a equals 10\n"; }
if ($a != $b) { print "a not equal to b\n"; }
if ($a < $b) { print "a less than b\n"; }
if ($b > $a) { print "b greater than a\n"; }

# String operators
my $str1 = "Hello";
my $str2 = "World";
my $concat = $str1 . " " . $str2;

if ($str1 eq "Hello") { print "String match\n"; }
if ($str1 ne $str2) { print "Strings differ\n"; }

# Logical operators
if ($a > 5 && $b > 15) {
    print "Both conditions true\n";
}

if ($a < 5 || $b > 15) {
    print "At least one condition true\n";
}
