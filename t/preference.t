#!/usr/bin/perl

use lib qw/t lib/;

use strict;
use warnings;
# use Games::Tournament::Swiss::Test -base; 
use Test::Base -base; 

use Games::Tournament::Contestant::Swiss::Preference;

filters qw/lines chomp/;
filters { input => [ qw/updatepref/ ], expected => [ qw/array/ ] };

plan tests => 1 * blocks;

run_is_deeply input => 'expected';

sub updatepref {
	my $sign = shift;
	my $difference = shift;
	# my $role = shift;
	my $oldRoles = shift;
	my @oldRoles = split /,/, $oldRoles;
	my @oldOld = @oldRoles;
	pop @oldOld;
	my $pref = Games::Tournament::Contestant::Swiss::Preference->new(
		sign => $sign, difference => $difference, lastTwo => \@oldOld);
	$pref->update( \@oldRoles );
	my $lastTwo = join ',', @{ $pref->lastTwo };
	return [ $pref->role, $pref->strength, $lastTwo ];
}

__END__

=== http://chesschat.org/showthread.php?t=7139 Player 5
--- input
White
2
White,White,None
--- expected
Black
Absolute
White,White

=== http://chesschat.org/showthread.php?t=7139 Player 5 again
--- input
White
0
Black,White,None
--- expected
Black
Mild
Black,White

=== http://chesschat.org/showthread.php?t=7139 Player 6
--- input
Black
1
Black,None,Black
--- expected
White
Absolute
Black,Black

=== http://chesschat.org/showthread.php?t=7139 Player 6 again
--- input
White
1
White,None,Black
--- expected
White
Mild
White,Black

=== http://chesschat.org/showthread.php?t=7139 Player 7
--- input
Black
1
None,Black,White
--- expected
Black
Mild
Black,White

=== http://chesschat.org/showthread.php?t=7139 Player 7 again
--- input
Black
1
None,Black,White
--- expected
Black
Mild
Black,White

=== http://chesschat.org/showthread.php?t=7139 Player 1
--- input
White
2
White,White,Black
--- expected
Black
Strong
White,Black

=== http://chesschat.org/showthread.php?t=7139 Player 1 again
--- input
White
0
Black,White,Black
--- expected
White
Strong
White,Black

=== http://chesschat.org/showthread.php?t=7139 Player 2
--- input
Black
2
Black,Black,White
--- expected
White
Strong
Black,White

=== http://chesschat.org/showthread.php?t=7139 Player 2 again
--- input
Black
0
White,Black,White
--- expected
Black
Strong
Black,White

=== http://chesschat.org/showthread.php?t=7139 Player 3
--- input
White
2
White,White,Black
--- expected
Black
Strong
White,Black

=== http://chesschat.org/showthread.php?t=7139 Player 3 again
--- input
White
0
Black,White,Black
--- expected
White
Strong
White,Black

=== http://chesschat.org/showthread.php?t=7139 Player 4
--- input
Black
2
Black,Black,White
--- expected
White
Strong
Black,White

=== http://chesschat.org/showthread.php?t=7139 Player 4 again
--- input
Black
0
White,Black,White
--- expected
Black
Strong
Black,White

=== Test 1
--- input
Black
0
Black,White
--- expected
Black
Strong
Black,White

=== Test 2
--- input
Black
0
White,Black
--- expected
White
Strong
White,Black

=== Test 3
--- input
Black
0
Black,Black
--- expected
White
Absolute
Black,Black

=== Test 4
--- input
Black
0
White,White
--- expected
Black
Absolute
White,White

=== Test 5
--- input
Black
1
Black,White
--- expected
Black
Mild
Black,White

=== Test 6
--- input
Black
1
White,Black
--- expected
White
Absolute
White,Black

=== Test 7
--- input
Black
1
Black,Black
--- expected
White
Absolute
Black,Black

=== Test 8
--- input
Black
1
White,White
--- expected
Black
Absolute
White,White

=== Test 9
--- input
Black
2
Black,White
--- expected
White
Strong
Black,White

=== Test 10
--- input
Black
2
White,Black
--- expected
White
Absolute
White,Black

=== Test 11
--- input
Black
2
Black,Black
--- expected
White
Absolute
Black,Black

=== Test 12
--- input
Black
2
White,White
--- expected
Black
Absolute
White,White

=== Test 13
--- input
Black
3
Black,White
--- expected
White
Absolute
Black,White

=== Test 14
--- input
Black
3
White,Black
--- expected
White
Absolute
White,Black

=== Test 15
--- input
Black
3
Black,Black
--- expected
White
Absolute
Black,Black

=== Test 16
--- input
Black
3
White,White
--- expected
Black
Absolute
White,White

=== Test 101
--- input
None
0
White
--- expected
Black
Strong
White

=== Test 102
--- input
White
1
White,Black
--- expected
White
Mild
White,Black

=== Test 103
--- input
Black
0
White,Black,White
--- expected
Black
Strong
Black,White

=== Test 104
--- input
White
1
White,Black,White,Black
--- expected
White
Mild
White,Black

=== Test 105
--- input
Black
0
White,Black,White,Black,White
--- expected
Black
Strong
Black,White

=== Test 106
--- input
White
1
White,Black,White,Black,White,Black
--- expected
White
Mild
White,Black

=== Test 107
--- input
Black
0
White,Black,White,Black,White,Black,White
--- expected
Black
Strong
Black,White

=== Test 108
--- input
White
1
White,Black,White,Black,White,Black,White,Black
--- expected
White
Mild
White,Black

=== Test 109
--- input
Black
0
White,Black,White,Black,White,Black,White,Black,White
--- expected
Black
Strong
Black,White

=== Test 110
--- input
White
1
White,Black,White,Black,White,Black,White,Black,White,Black
--- expected
White
Mild
White,Black

=== Test 111
--- input
Black
0
White,Black,White,Black,White,Black,White,Black,White,Black,White
--- expected
Black
Strong
Black,White

=== Test 112
--- input
White
1
White,Black,White,Black,White,Black,White,Black,White,Black,White,Black
--- expected
White
Mild
White,Black

=== Test 113
--- input
Black
0
White,Black,White,Black,White,Black,White,Black,White,Black,White,Black,White
--- expected
Black
Strong
Black,White

=== Test 114
--- input
White
1
White,Black
--- expected
White
Mild
White,Black

=== Test 115
--- input
Black
0
Black,White
--- expected
Black
Strong
Black,White
