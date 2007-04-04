#!usr/bin/perl

use lib qw/t lib/;

use strict;
use warnings;
use Test::More;
use YAML;

BEGIN {
    $Games::Tournament::Swiss::Config::firstround = 1;
    @Games::Tournament::Swiss::Config::roles      = qw/Black White/;
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

my $p = Games::Tournament::Swiss->new(
    rounds   => 3,
    entrants => [ $antonio, $tiago, $bruno, $paulo ]
);

$p->round(0);

$p->assignPairingNumbers;
$p->initializePreferences;
$p->initializePreferences until $antonio->preference->role eq 'White';

my @b = $p->formBrackets;

my $pairing  = $p->pairing( \@b );
my %p        = $pairing->matchPlayers;
my @m = @{ $p{matches} };
my @nextGame = map { @{$_} } @m;

my $t;
my @tests = (
[ $m[0][0]->isa('Games::Tournament::Card'),	'$m0 isa'],
[ $m[0][1]->isa('Games::Tournament::Card'),	'$m1 isa'],
[ do {grep {$antonio == $_} $m[0][0]->myPlayers},	'$m0 participant1'],
[ do {grep {$tiago == $_} $m[0][0]->myPlayers},	'$m0 participant2'],
[ do {grep {$bruno == $_} $m[0][1]->myPlayers},	'$m1 participant1'],
[ do {grep {$paulo == $_} $m[0][1]->myPlayers},	'$m1 participant2'],
);

plan tests => $#tests + 1;

map { ok( $_->[0], $_->[ 1, ], ) } @tests;
