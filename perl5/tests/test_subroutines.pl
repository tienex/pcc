#!/usr/bin/env perl
# Test: Subroutine definitions and calls

sub greet {
    my ($name) = @_;
    print "Hello, $name!\n";
}

sub add {
    my ($a, $b) = @_;
    return $a + $b;
}

greet("World");
my $sum = add(5, 7);
print "Sum: $sum\n";
