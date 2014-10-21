#!usr/bin/perl

use lib qw/t lib/;

use strict;
use warnings;
use Test::More;
use YAML;

BEGIN {
    $Games::Tournament::Swiss::Config::firstround = 1;
    @Games::Tournament::Swiss::Config::roles      = qw/Black White/;
    %Games::Tournament::Swiss::Config::scores      = (
    Win => 1, Draw => 0.5, Loss => 0, Absence => 0 );
    $Games::Tournament::Swiss::Config::algorithm  =
      'Games::Tournament::Swiss::Procedure::FIDE';
}
use Games::Tournament::Contestant::Swiss;
use Games::Tournament::Swiss;
use Games::Tournament::Card;

my @members = Load(<<'...');
---
id: 1
name: Antonio, Hobmeir Neto
rating: 12
title: Unknown
---
id: 2
name: Tiago, Vignatti
rating: 8
title: Unknown
---
id: 3
name: Bruno, Ribas
rating: 4
title: Unknown
---
id: 4
name: Paulo, Zanoni
rating: 1
title: Unknown
...

my ($antonio, $tiago, $bruno, $paulo)
	= map { Games::Tournament::Contestant::Swiss->new(%$_) } @members;

my $t = Games::Tournament::Swiss->new(
    rounds   => 3,
    entrants => [ $antonio, $tiago, $bruno, $paulo ]
);

$t->round(0);
$t->assignPairingNumbers;
$t->initializePreferences;
$t->initializePreferences until $antonio->preference->role eq 'White';

my @b = $t->formBrackets;
my $pairing  = $t->pairing( \@b );
my %p        = $pairing->matchPlayers;
my @m = @{ $p{matches} };
$t->round(1);

my @tests = (
[ $m[0][0]->isa('Games::Tournament::Card'),	'$m0 isa'],
[ $m[0][1]->isa('Games::Tournament::Card'),	'$m1 isa'],
[ $antonio == $m[0][0]->contestants->{White},	'$m0 White'],
[ $bruno == $m[0][0]->contestants->{Black},	'$m0 Black'],
[ $tiago == $m[0][1]->contestants->{Black},	'$m1 White'],
[ $paulo == $m[0][1]->contestants->{White},	'$m1 Black'],
);

my @matches = map { @$_ } @m;
for my $match ( @matches )
{
	$match->result({Black => 'Draw', White => 'Draw' });
}
$t->collectCards( @matches );
my @b2 = $t->formBrackets;
my $p2  = $t->pairing( \@b2 );
my %p2        = $p2->matchPlayers;
my @m2 = @{ $p2{matches} };
$t->round(2);

push @tests, (
[ $m2[0][0]->isa('Games::Tournament::Card'),	'@m2 isa'],
[ $m2[0][1]->isa('Games::Tournament::Card'),	'@m2 isa'],
[ $tiago == $m2[0][0]->contestants->{White},	'@m2 White0'],
[ $antonio == $m2[0][0]->contestants->{Black},	'@m2 Black0'],
[ $bruno == $m2[0][1]->contestants->{White},	'@m2 White1'],
[ $paulo == $m2[0][1]->contestants->{Black},	'@m2 Black1'],
);

my @matches2 = map { @$_ } @m2;
for my $match ( @matches2 )
{
	$match->result({Black => 'Draw', White => 'Draw' });
}
$t->collectCards( @matches2 );
my @b3 = $t->formBrackets;
my $p3 = $t->pairing( \@b3 );
my %p3 = $p3->matchPlayers;
my @m3 = @{ $p3{matches} };
$t->round(3);

push @tests, (
[ $m3[0][0]->isa('Games::Tournament::Card'),	'@m3 isa'],
[ $m3[0][1]->isa('Games::Tournament::Card'),	'@m3 isa'],
[ $antonio == $m3[0][0]->contestants->{White},	'@m3 White0'],
[ $paulo == $m3[0][0]->contestants->{Black},	'@m3 Black0'],
[ $bruno == $m3[0][1]->contestants->{White},	'@m3 White1'],
[ $tiago == $m3[0][1]->contestants->{Black},	'@m3 Black1'],
);

plan tests => $#tests + 1;

map { ok( $_->[0], $_->[ 1, ], ) } @tests;
