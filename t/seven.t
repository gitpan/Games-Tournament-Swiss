#!usr/bin/perl

# 7 players who neither win nor lose any match. ie all are draws.
# TODO pairing 4th round produces doubled players!

use lib qw/t lib/;

use strict;
use warnings;
use Test::More;
use YAML;

BEGIN {
    $Games::Tournament::Swiss::Config::firstround = 1;
    @Games::Tournament::Swiss::Config::roles      = qw/Black White/;
    %Games::Tournament::Swiss::Config::scores      = (
    Win => 1, Draw => 0.5, Loss => 0, Absence => 0, Bye => 1 );
    $Games::Tournament::Swiss::Config::algorithm  =
      'Games::Tournament::Swiss::Procedure::FIDE';
}
use Games::Tournament::Contestant::Swiss;
use Games::Tournament::Swiss;
use Games::Tournament::Card;

my @members = Load(<<'...');
---
id: 1
name: One
rating: 12
title: Unknown
---
id: 2
name: Two
rating: 8
title: Unknown
---
id: 3
name: Three
rating: 4
title: Unknown
---
id: 4
name: Four
rating: 3
title: Unknown
---
id: 5
name: Five
rating: 2
title: Unknown
---
id: 6
name: Six
rating: 1
title: Unknown
---
id: 7
name: Seven
rating: 0
title: Unknown
...

my ($one, $two, $three, $four, $five, $six, $seven)
	= map { Games::Tournament::Contestant::Swiss->new(%$_) } @members;

my $t = Games::Tournament::Swiss->new(
    rounds   => 3,
    entrants => [$one, $two, $three, $four, $five, $six, $seven]
);

$t->round(0);
$t->assignPairingNumbers;
$t->initializePreferences;
$t->initializePreferences until $one->preference->role eq 'White';

my @b = $t->formBrackets;
my $pairing  = $t->pairing( \@b );
my %p        = $pairing->matchPlayers;
my @m = map { @$_ } @{ $p{matches} };
$t->round(1);

my @tests = (
[ $m[0]->isa('Games::Tournament::Card'),	'$m0 isa'],
[ $m[1]->isa('Games::Tournament::Card'),	'$m1 isa'],
[ $m[2]->isa('Games::Tournament::Card'),	'$m1 isa'],
[ $m[3]->isa('Games::Tournament::Card'),	'$m1 isa'],
[ $one == $m[0]->contestants->{White},	'$m0 White'],
[ $four == $m[0]->contestants->{Black},	'$m0 Black'],
[ $two == $m[1]->contestants->{Black},	'$m1 Black'],
[ $five == $m[1]->contestants->{White},	'$m1 White'],
[ $three == $m[2]->contestants->{White},	'$m2 White'],
[ $six == $m[2]->contestants->{Black},	'$m2 Black'],
[ $seven == $m[3]->contestants->{Bye},	'$m3 Bye'],
);

for my $match ( @m )
{
	$match->result({Black => 'Draw', White => 'Draw' }) 
			unless $match->result;
}
$t->collectCards( @m );
my @b2 = $t->formBrackets;
my $p2  = $t->pairing( \@b2 );
my %p2        = $p2->matchPlayers;
my @m2 = map { @$_ } @{ $p2{matches} };
$t->round(2);

push @tests, (
[ $m2[0]->isa('Games::Tournament::Card'),	'@m2 isa'],
[ $m2[1]->isa('Games::Tournament::Card'),	'@m2 isa'],
[ $m2[2]->isa('Games::Tournament::Card'),	'@m2 isa'],
[ $m2[3]->isa('Games::Tournament::Card'),	'@m2 isa'],
[ $seven == $m2[0]->contestants->{White},	'@m2 White0'],
[ $one == $m2[0]->contestants->{Black},	'@m2 Black0'],
[ $three == $m2[1]->contestants->{Black},	'@m2 Black1'],
[ $two == $m2[1]->contestants->{White},	'@m2 White1'],
[ $four == $m2[2]->contestants->{White},	'@m2 White2'],
[ $five == $m2[2]->contestants->{Black},	'@m2 Black2'],
[ $six == $m2[3]->contestants->{Bye},	'@m2 Bye'],
);

for my $match ( @m2 )
{
	$match->result({Black => 'Draw', White => 'Draw' })
			unless $match->result;
}
$t->collectCards( @m2 );
my @b3 = $t->formBrackets;
my $p3 = $t->pairing( \@b3 );
my %p3 = $p3->matchPlayers;
my @m3 = map { @$_ } @{ $p3{matches} };
$t->round(3);

push @tests, (
[ $m3[0]->isa('Games::Tournament::Card'),	'@m3 isa'],
[ $m3[1]->isa('Games::Tournament::Card'),	'@m3 isa'],
[ $m3[2]->isa('Games::Tournament::Card'),	'@m3 isa'],
[ $m3[3]->isa('Games::Tournament::Card'),	'@m3 isa'],
[ $six == $m3[0]->contestants->{White},	'@m3 White0'],
[ $seven == $m3[0]->contestants->{Black},	'@m3 Black0'],
[ $one == $m3[1]->contestants->{White},	'@m3 White1'],
[ $two == $m3[1]->contestants->{Black},	'@m3 Black1'],
[ $three == $m3[2]->contestants->{White},	'@m3 White2'],
[ $four == $m3[2]->contestants->{Black},	'@m3 Black2'],
[ $five == $m3[3]->contestants->{Bye},	'@m3 Bye'],
);

=begin comment text

TODO: {
	for my $match ( @m3 )
	{
		$match->result({Black => 'Draw', White => 'Draw' })
				unless $match->result;
	}
	$t->collectCards( @m3 );
	my @b4 = $t->formBrackets;
	my $p4 = $t->pairing( \@b4 );
	my %p4 = $p4->matchPlayers;
	my @m4 = map { @$_ } @{ $p4{matches} };
	$t->round(4);

	push @tests, (
	[ $m4[0]->isa('Games::Tournament::Card'),	'@m4 isa'],
	[ $m4[1]->isa('Games::Tournament::Card'),	'@m4 isa'],
	[ $m4[2]->isa('Games::Tournament::Card'),	'@m4 isa'],
	[ $m4[3]->isa('Games::Tournament::Card'),	'@m4 isa'],
	[ $four == $m4[0]->contestants->{White},	'@m4 White0'],
	[ $seven == $m4[0]->contestants->{Black},	'@m4 Black0'],
	[ $three == $m4[1]->contestants->{Black},	'@m4 Black1'],
	[ $one == $m4[1]->contestants->{White},	'@m4 White1'],
	[ $six == $m4[2]->contestants->{White},	'@m4 White2'],
	[ $two == $m4[2]->contestants->{Black},	'@m4 Black2'],
	[ $five == $m4[3]->contestants->{Bye},	'@m4 Bye'],
	);
}

=end comment text

=cut

plan tests => $#tests + 1;

map { ok( $_->[0], $_->[ 1, ], ) } @tests;
