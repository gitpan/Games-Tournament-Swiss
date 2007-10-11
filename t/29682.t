#!usr/bin/perl

# http://rt.cpan.org/Ticket/Display.html?id=29682
# floating pairing checks failed in 0.09

use lib qw/t lib/;

use strict;
use warnings;
use Test::More;
use YAML;

BEGIN {
    $Games::Tournament::Swiss::Config::firstround = 1;
    @Games::Tournament::Swiss::Config::roles      = qw/White Black/;
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
name: 'Bartolomäus,Chri'
rating: 2810
title: Unknown
---
id: 2
name: 'Sydow, Hardy'
rating: 2800
title: Unknown
---
id: 3
name: 'Prüsse, Horst'
rating: 2780
title: Unknown
---
id: 4
name: 'Kliewe, Hans - J'
rating: 2775
title: Unknown
---
id: 5
name: 'Zentgraf,Robert'
rating: 2770
title: Unknown
---
id: 6
name: 'Helms, Sven'
rating: 2760
title: Unknown
---
id: 7
name: 'Bauer, Norbert'
rating: 2750
title: Unknown
---
id: 8
name: 'Werner, Bernd- M'
rating: 2740
title: Unknown
---
id: 9
name: 'Kutschke, Peter'
rating: 2735
title: Unknown
---
id: 10
name: 'Baier, Karsten'
rating: 2730
title: Unknown
---
id: 11
name: 'Zoll, Detlef'
rating: 2725
title: Unknown
---
id: 12
name: 'Sager, Bernd'
rating: 2720
title: Unknown
---
id: 13
name: 'Zimmermann, Gord'
rating: 2715
title: Unknown
---
id: 14
name: 'Böttcher, Dr. Fr'
rating: 2712
title: Unknown
---
id: 15
name: 'Graffenberger, M'
rating: 2710
title: Unknown
---
id: 16
name: 'Huhnstock, Rico'
rating: 2680
title: Unknown
---
id: 17
name: 'Baumgarten, Thom'
rating: 2660
title: Unknown
---
id: 18
name: 'Zibell, Walter'
rating: 2650
title: Unknown
---
id: 19
name: 'Hauff, Andre'
rating: 2640
title: Unknown
---
id: 20
name: 'Springer, Guido'
rating: 2638
title: Unknown
...

my @lineup
	= map { Games::Tournament::Contestant::Swiss->new(%$_) } @members;

my $tourney = Games::Tournament::Swiss->new( entrants => \@lineup);

my $round = 2;
$tourney->round($round);
$tourney->assignPairingNumbers;
$tourney->initializePreferences;
$tourney->initializePreferences until $lineup[0]->preference->role eq 'White';

my @ids = map { $_->{pairingNumber} } @lineup;
my $pairingtable = Load(<<'...');
---
floats:
  1: [ ~, Up ]
  10: [ ~, Down ]
  11: [ ~, Down ]
  12: [ ~, Up ]
  13: [ ~, ~ ]
  14: [ ~, ~ ]
  15: [ ~, ~ ]
  16: [ ~, ~ ]
  17: [ ~, ~ ]
  18: [ ~, ~ ]
  19: [ ~, ~ ]
  2: [ ~, ~ ]
  20: [ ~, ~ ]
  3: [ ~, ~ ]
  4: [ ~, ~ ]
  5: [ ~, ~ ]
  6: [ ~, ~ ]
  7: [ ~, ~ ]
  8: [ ~, ~ ]
  9: [ ~, ~ ]
opponents:
  1: [ 11, 10 ]
  10: [ 20, 1 ]
  11: [ 1, 12 ]
  12: [ 2, 11 ]
  13: [ 3, 18 ]
  14: [ 4, 17 ]
  15: [ 5, 20 ]
  16: [ 6, 19 ]
  17: [ 7, 14 ]
  18: [ 8, 13 ]
  19: [ 9, 16 ]
  2: [ 12, 7 ]
  20: [ 10, 15 ]
  3: [ 13, 6 ]
  4: [ 14, 9 ]
  5: [ 15, 8 ]
  6: [ 16, 3 ]
  7: [ 17, 2 ]
  8: [ 18, 5 ]
  9: [ 19, 4 ]
roles:
  1: [ White, Black ]
  10: [ Black, White ]
  11: [ Black, White ]
  12: [ White, Black ]
  13: [ Black, White ]
  14: [ White, Black ]
  15: [ Black, White ]
  16: [ White, Black ]
  17: [ Black, White ]
  18: [ White, Black ]
  19: [ Black, White ]
  2: [ Black, White ]
  20: [ White, Black ]
  3: [ White, Black ]
  4: [ Black, White ]
  5: [ White, Black ]
  6: [ Black, White ]
  7: [ White, Black ]
  8: [ Black, White ]
  9: [ White, Black ]
score:
  1: 1.5
  10: 1
  11: 1
  12: 0.5
  13: 1
  14: 0
  15: 1
  16: 1
  17: 1
  18: 0
  19: 0
  2: 1.5
  20: 0
  3: 1.5
  4: 1.5
  5: 1
  6: 1.5
  7: 1.5
  8: 2
  9: 1.5
...

my ( $opponents, $roles, $floats, $score ) = 
    @$pairingtable{qw/opponents roles floats score/};
for my $player ( @lineup )
{
    my $id = $player->id;
    $player->score( $score->{$id} );
}
my $lastround = $round;
for my $round ( 1..$lastround )
{
   my (%games, @games);
   for my $id ( @ids )
   {
	next if $games{$id};
	my $player = $tourney->ided($id);
	my $opponentId = $opponents->{$id}->[$round-1];
	my $opponent = $tourney->ided($opponentId);
	my $role = $roles->{$id}->[$round-1];
	my $opponentRole = $roles->{$opponentId}->[$round-1];
	my $game = Games::Tournament::Card->new(
	    round => $round,
	    contestants => { $role => $player, $opponentRole => $opponent} );
        if ($round >= $lastround-1)
        {
	    my $float = $floats->{$id}->[$round-$lastround-1];
	    my $opponentFloat = $floats->{$opponent}->[$round-$lastround-1];
	    $game->float($player, $float);
	    $game->float($opponent, $opponentFloat);
        }
        $games{$id} = $game;
        $games{$opponentId} = $game;
        push @games, $game;
   }
   $tourney->collectCards( @games );
}

my %b = $tourney->formBrackets;
my $pairing  = $tourney->pairing( \%b );
my $p        = $pairing->matchPlayers;
my %m = map { $_ => $p->{matches}->{$_} } keys %{ $p->{matches} };
$tourney->round(5);

my @tests = (
[ $m{1.5}->[0]->isa('Games::Tournament::Card'),	'$m1.5 isa'],
[ $m{'1.5Remainder'}->[0]->isa('Games::Tournament::Card'),	'$m1.5 isa'],
[ $m{'1.5Remainder'}->[1]->isa('Games::Tournament::Card'),	'$m1.5R isa'],
[ $m{'1.5Remainder'}->[2]->isa('Games::Tournament::Card'),	'$m1.5R isa'],
[ $m{1}->[0]->isa('Games::Tournament::Card'),	'$m1 isa'],
[ $m{1}->[1]->isa('Games::Tournament::Card'),	'$m1 isa'],
[ $m{1}->[2]->isa('Games::Tournament::Card'),	'$m1 isa'],
[ $m{0.5}->[0]->isa('Games::Tournament::Card'),	'$m0.5 isa'],
[ $m{0}->[0]->isa('Games::Tournament::Card'),	'$m0 isa'],
[ $m{0}->[1]->isa('Games::Tournament::Card'),	'$m0 isa'],
);

push @tests,
[ $lineup[7],	$m{1.5}->[0]->contestants->{Black},	'$m1.5 Black'],
[ $lineup[2],	$m{1.5}->[0]->contestants->{White},	'$m1.5 White'],
[ $lineup[5],	$m{'1.5Remainder'}->[0]->contestants->{Black},	'$m1.5R Black'],
[ $lineup[0],	$m{'1.5Remainder'}->[0]->contestants->{White},	'$m1.5R White'],
[ $lineup[1],	$m{'1.5Remainder'}->[1]->contestants->{Black},	'$m1.5R Black'],
[ $lineup[8],	$m{'1.5Remainder'}->[1]->contestants->{White},	'$m1.5R White'],
[ $lineup[3],	$m{'1.5Remainder'}->[2]->contestants->{Black},	'$m1.5R Black'],
[ $lineup[6],	$m{'1.5Remainder'}->[2]->contestants->{White},	'$m1.5R White'],
[ $lineup[12],	$m{1}->[0]->contestants->{Black},	'$m1 Black'],
[ $lineup[4],	$m{1}->[0]->contestants->{White},	'$m1 White'],
[ $lineup[9],	$m{1}->[1]->contestants->{Black},	'$m1 Black'],
[ $lineup[14],	$m{1}->[1]->contestants->{White},	'$m1 White'],
[ $lineup[10],	$m{1}->[2]->contestants->{Black},	'$m1 Black'],
[ $lineup[15],	$m{1}->[2]->contestants->{White},	'$m1 White'],
[ $lineup[16],	$m{0.5}->[0]->contestants->{Black},	'$m0.5 Black'],
[ $lineup[11],	$m{0.5}->[0]->contestants->{White},	'$m0.5 White'],
[ $lineup[18],	$m{0}->[0]->contestants->{Black},	'$m0 Black'],
[ $lineup[13],	$m{0}->[0]->contestants->{White},	'$m0 White'],
[ $lineup[19],	$m{0}->[1]->contestants->{Black},	'$m0 Black'],
[ $lineup[17],	$m{0}->[1]->contestants->{White},	'$m0 White'],
;

plan tests => $#tests + 1;

ok( $_->[0], $_->[ 1, ], ) for @tests[0..9];
is_deeply( $_->[0], $_->[ 1, ], $_->[ 2, ], ) for @tests[10..$#tests];

# vim: set ts=8 sts=4 sw=4 noet:
