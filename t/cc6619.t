#!usr/bin/perl

# http://chesschat.org/showthread.php?t=6619
# the pairings accepted in this test are disputable

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
name: One
rating: 10
title: Unknown
---
id: 2
name: Two
rating: 9
title: Unknown
---
id: 3
name: Three
rating: 8
title: Unknown
---
id: 4
name: Four
rating: 7
title: Unknown
---
id: 5
name: Five
rating: 6
title: Unknown
---
id: 6
name: Six
rating: 5
title: Unknown
---
id: 7
name: Seven
rating: 4
title: Unknown
---
id: 8
name: Eight
rating: 3
title: Unknown
---
id: 9
name: Nine
rating: 2
title: Unknown
---
id: 10
name: Ten
rating: 1
title: Unknown
...

my ($one, $two, $three, $four, $five, $six, $seven, $eight, $nine, $ten)
	= map { Games::Tournament::Contestant::Swiss->new(%$_) } @members;
my @lineup =
    ($one, $two, $three, $four, $five, $six, $seven, $eight, $nine, $ten);

my $tourney = Games::Tournament::Swiss->new( entrants => \@lineup);

my $round = 4;
$tourney->round($round);
$tourney->assignPairingNumbers;
$tourney->initializePreferences;
$tourney->initializePreferences until $one->preference->role eq 'White';

my @ids = map { $_->{pairingNumber} } @lineup;
my $pairingtable = Load(<<'...');
---
opponents:
  1:  [6,4,2,5 ]
  2:  [7,3,1,4 ]
  3:  [8,2,6,7 ]
  6:  [1,5,3,9 ]
  4:  [9,1,7,2 ]
  5:  [10,6,8,1]
  8:  [3,9,5,10]
  7:  [2,10,4,3]
  9:  [4,8,10,6]
  10: [5,7,9,8 ]
roles:
  1 : [White,Black,White,Black]
  2 : [Black,White,Black,White]
  3 : [White,Black,White,Black]
  6 : [Black,White,Black,White]
  4 : [Black,White,Black,Black]
  5 : [White,Black,White,White]
  8 : [Black,White,Black,White]
  7 : [White,Black,White,White]
  9 : [White,Black,White,Black]
  10: [Black,White,Black,Black]
floats:
  1 : [~,D]
  2 : [~,D]
  3 : [~,D]
  6 : [~,D]
  4 : [~,U]
  5 : [~,U]
  8 : [~,D]
  7 : [~,U]
  9 : [~,U]
  10: [~,U]
score:
  1 : 3.5
  2 : 3.5
  3 : 2.5
  6 : 2.5
  4 : 2  
  5 : 2  
  8 : 2  
  7 : 1  
  9 : 1  
  10: 0  
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

my @b = $tourney->formBrackets;
my $pairing  = $tourney->pairing( \@b );
my %p        = $pairing->matchPlayers;
my @m = map { @$_ } @{ $p{matches} };
$tourney->round(5);

my @tests = (
[ $m[0]->isa('Games::Tournament::Card'),	'$m0 isa'],
[ $m[1]->isa('Games::Tournament::Card'),	'$m1 isa'],
[ $m[2]->isa('Games::Tournament::Card'),	'$m2 isa'],
[ $m[3]->isa('Games::Tournament::Card'),	'$m3 isa'],
[ $one == $m[0]->contestants->{White},	'$m0 White'],
[ $three == $m[0]->contestants->{Black},	'$m0 Black'],
[ $ten == $m[1]->contestants->{White},	'$m0 White'],
[ $two == $m[1]->contestants->{Black},	'$m0 Black'],
[ $six == $m[2]->contestants->{White},	'$m1 White'],
[ $seven == $m[2]->contestants->{Black},	'$m1 Black'],
[ $four == $m[3]->contestants->{White},	'$m2 White'],
[ $eight == $m[3]->contestants->{Black},	'$m2 Black'],
[ $nine == $m[4]->contestants->{White},	'$m3 White'],
[ $five == $m[4]->contestants->{Black},	'$m3 Black'],
);

plan tests => $#tests + 1;

map { ok( $_->[0], $_->[ 1, ], ) } @tests;

# vim: set ts=8 sts=4 sw=4 noet:
