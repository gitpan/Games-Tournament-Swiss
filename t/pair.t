#!usr/bin/perl

use lib qw/t lib/;

use strict;
use warnings;
use Test::More;

BEGIN {
    $Games::Tournament::Swiss::Config::firstround = 1;
    @Games::Tournament::Swiss::Config::roles      = qw/Black White/;
    $Games::Tournament::Swiss::Config::algorithm  =
      'Games::Tournament::Swiss::Procedure::Dummy';
}
use Games::Tournament::Contestant::Swiss;
use Games::Tournament::Swiss;
use Games::Tournament::Card;

my $a = Games::Tournament::Contestant::Swiss->new(
    id     => 1,
    name   => 'Ros',
    title  => 'Expert',
    rating => 100,
);
my $b = Games::Tournament::Contestant::Swiss->new(
    id     => 2,
    name   => 'Ron',
    title  => 'Expert',
    rating => 80,
);
my $c = Games::Tournament::Contestant::Swiss->new(
    id     => 3,
    name   => 'Rog',
    score  => 3,
    title  => 'Expert',
    rating => '50',
);
my $d = Games::Tournament::Contestant::Swiss->new(
    id     => 4,
    name   => 'Ray',
    title  => 'Novice',
    rating => 25,
);
my $e = Games::Tournament::Contestant::Swiss->new(
    id     => 5,
    name   => 'Rob',
    score  => 3,
    title  => 'Novice',
    rating => 1,
);
my $f = Games::Tournament::Contestant::Swiss->new(
    id     => 6,
    name   => 'Rod',
    score  => 3,
    title  => 'Novice',
    rating => 0,
);
my $g = Games::Tournament::Contestant::Swiss->new(
    id    => 7,
    name  => 'Reg',
    score => 3,
    title => 'Novice',
);
my $h = Games::Tournament::Contestant::Swiss->new(
    id    => 8,
    name  => 'Red',
    score => 3,
    title => 'Novice',
);
my $i = Games::Tournament::Contestant::Swiss->new(
    id    => 9,
    name  => 'Roy',
    score => 3,
    title => 'Novice',
);

my $p = Games::Tournament::Swiss->new(
    rounds   => 3,
    entrants => [ $a, $b, $c, $d, $e, $f, $g, $h, $i ]
);

$p->round(0);

$p->assignPairingNumbers;
$p->initializePreferences;

my @b = $p->formBrackets;

my $pairing  = $p->pairing( \@b );
my $m        = $pairing->matchPlayers;
my @nextGame = map { @{$_} } @{$m};

my $t;
my @tests = (
[ $m->[0]->[0]->isa('Games::Tournament::Card'),	'$m0 isa'],
[ $m->[0]->[1]->isa('Games::Tournament::Card'),	'$m1 isa'],
[ $m->[0]->[2]->isa('Games::Tournament::Card'),	'$m2 isa'],
[ $m->[0]->[3]->isa('Games::Tournament::Card'),	'$m3 isa'],
[ $m->[0]->[4]->isa('Games::Tournament::Card'),	'$m4 isa'],
[ do {grep {$a == $_} $m->[0]->[0]->myPlayers},	'$m0 participant1'],
[ do {grep {$e == $_} $m->[0]->[0]->myPlayers},	'$m0 participant2'],
[ do {grep {$b == $_} $m->[0]->[1]->myPlayers},	'$m1 participant1'],
[ do {grep {$f == $_} $m->[0]->[1]->myPlayers},	'$m1 participant2'],
[ do {grep {$c == $_} $m->[0]->[2]->myPlayers},	'$m2 participant1'],
[ do {grep {$g == $_} $m->[0]->[2]->myPlayers},	'$m2 participant2'],
[ do {grep {$d == $_} $m->[0]->[3]->myPlayers},	'$m3 participant1'],
[ do {grep {$h == $_} $m->[0]->[3]->myPlayers},	'$m3 participant2'],
[ do {grep {$i == $_} $m->[0]->[4]->myPlayers},	'$m4 byer'],
);

plan tests => $#tests + 1;

map { ok( $_->[0], $_->[ 1, ], ) } @tests;
