#!usr/bin/perl

# In the http://chesschat.org/showthread.php?t=6619 thread,
# a second, similar pairing table
# http://chesschat.org/showpost.php?p=169490&postcount=52
# like the first, the pairings accepted in this test are also disputable

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

my $n = 10;
my ($one, $two, $three, $four, $five, $six, $seven, $eight, $nine, $ten)
	= map { Games::Tournament::Contestant::Swiss->new(
	id => 11-$n, name => $_, rating => $n--, title => 'Nom') } ('A'..'J');
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
floats:
  1: [~,Down]
  10: [~,Up]
  2: [~,Down]
  3: [~,Up]
  4: [~,Down]
  5: [~,Up]
  6: [~,Down]
  7: [~,~]
  8: [~,~]
  9: [~,Up]
opponents:
  1: [6,4,2,3]
  10: [5,7,9,4]
  2: [7,3,1,5]
  3: [8,2,6,1]
  4: [9,1,7,10]
  5: [10,6,8,2]
  6: [1,5,3,9]
  7: [2,10,4,8]
  8: [3,9,5,7]
  9: [4,8,10,6]
roles:
  1: [Black,White,Black,White]
  10: [White,Black,White,White]
  2: [White,Black,White,Black]
  3: [Black,White,Black,Black]
  4: [White,Black,White,Black]
  5: [Black,White,Black,White]
  6: [White,Black,White,Black]
  7: [Black,White,Black,White]
  8: [White,Black,White,Black]
  9: [Black,White,Black,White]
score:
  1: 3.5
  10: 1
  2: 3.5
  3: 2
  4: 3
  5: 2
  6: 1
  7: 2
  8: 1
  9: 1
...

my ( $floats, $opponents, $roles, $score ) = 
    @$pairingtable{qw/floats opponents roles score/};
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
my %m = %{ $p->{matches} };
$tourney->round(5);

# Round 5:  1 2 (3.5), 4 (3), 3 5 7 (2), 6 8 9 10 (1),

# SwissPerfect pairings, straight by book.

my @tests = (
[ $m{3}->[0]->isa('Games::Tournament::Card'),	'$m0 isa'],
[ $m{1}->[0]->isa('Games::Tournament::Card'),	'$m0 isa'],
[ $m{1}->[1]->isa('Games::Tournament::Card'),	'$m1 isa'],
[ $m{1}->[2]->isa('Games::Tournament::Card'),	'$m2 isa'],
[ $m{1}->[3]->isa('Games::Tournament::Card'),	'$m3 isa'],
[ $two == $m{3}->[0]->contestants->{White},	'$m0 White'],
[ $four == $m{3}->[0]->contestants->{Black},	'$m0 Black'],
[ $eight == $m{1}->[0]->contestants->{White},	'$m0 White'],
[ $one == $m{1}->[0]->contestants->{Black},	'$m0 Black'],
[ $three == $m{1}->[1]->contestants->{White},	'$m1 White'],
[ $ten == $m{1}->[1]->contestants->{Black},	'$m1 Black'],
[ $nine == $m{1}->[2]->contestants->{White},	'$m2 White'],
[ $five == $m{1}->[2]->contestants->{Black},	'$m2 Black'],
[ $six == $m{1}->[3]->contestants->{White},	'$m3 White'],
[ $seven == $m{1}->[3]->contestants->{Black},	'$m3 Black'],
);

plan tests => $#tests + 1;

map { ok( $_->[0], $_->[ 1, ], ) } @tests;

# vim: set ts=8 sts=4 sw=4 noet:
