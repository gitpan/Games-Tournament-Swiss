package Games::Tournament::Swiss;

# Last Edit: 2007 Sep 01, 09:08:47 AM
# $Id: $

use warnings;
use strict;
use Carp;

use Games::Tournament::Swiss::Config;

use constant ROLES      => @Games::Tournament::Swiss::Config::roles;
use constant FIRSTROUND => $Games::Tournament::Swiss::Config::firstround;

use base qw/Games::Tournament/;

# use Games::Tournament::Swiss::Bracket;
#use Games::Tournament::Contestant::Swiss -mixin =>
#  qw/score scores rating title name pairingNumber oldId roles/;
use Games::Tournament::Contestant::Swiss;
use Games::Tournament::Swiss::Procedure;
use Games::Tournament::Contestant::Swiss::Preference;

use List::Util qw/min reduce sum/;

=head1 NAME

Games::Tournament::Swiss - FIDE Swiss Same-Rank Contestant Pairing 

=head1 VERSION

Version 0.06

=cut

our $VERSION = '0.06';

=head1 SYNOPSIS

    @Games::Tournament::Swiss::roles = qw/Black White/;
    $tourney = Games::Tournament::Swiss->new($rounds, \@entrants);
    @rankedPlayers = $tourney->assignPairingNumbers;
    $tourney->initializePreferences;


    ...

    $tourney->collectCards(@games);
    @scores = $tourney->updateScores($round);
    @groups = $tourney->formBrackets($round);
    $round5 = $tourney->pairing( \@groups );
    $matches = $round5->matchPlayers;
    $round5->allocateColors;

=head1 DESCRIPTION

In a Swiss tournament, there is a pre-declared number of rounds, each contestant meets every other contestant zero or one times, and in each round contestants are paired with other players with the same, or similar, scores.

=head1 METHODS

=head2 assignPairingNumbers

 @rankings = $tourney->assignPairingNumbers;

Sets the participants pairing numbers, sorting on rating, title and name, and substitutes this for the id they had before (The old id is saved as oldId.) If a pairingNumber attribute already exists for all @players, however, this is used instead, and the old id isn't saved. This function uses Games::Tournament::rank. Before the first round, all scores are usually 0. But if they *do* have scores, they are not used to rank. A2

=cut

sub assignPairingNumbers {
    my $self    = shift;
    my @players = @{ $self->{entrants} };
    if ( @players == grep { $_->{pairingNumber} } @players ) {
        for (@players) { $_->{id} ||= $_->{pairingNumber}; }
        return @players;
    }
    my @realScores;
    my $n = 0;
    for (@players) { $realScores[$n] = $_->scores if defined $_->scores; $n++ }
    $_->{scores} = undef foreach @players;
    my @rankings = $self->rank(@players);
    foreach my $n ( 0 .. $#players ) {
        $players[$n]->scores( $realScores[$n] );
        $rankings[$n]->pairingNumber( $n + 1 );
        $rankings[$n]->oldId( $rankings[$n]->id ) unless $rankings[$n]->oldId;
        $rankings[$n]->id( $rankings[$n]->pairingNumber );
    }
    $self->entrants( \@rankings );
}


=head2 initializePreferences

 @rankings = $tourney->initializePreferences;

Before the first round, the color (role) preference of the highest ranked player and the other odd-numbered players in the top half of the rankings is determined by lot. The preference of the even-numbered players in the top half is given to the other color.  E5

=cut

sub initializePreferences {
    my $self    = shift;
    my @players = @{ $self->{entrants} };
    my ( $evenRole, $oddRole ) = $self->randomRole;
    $_->preference( Games::Tournament::Contestant::Swiss::Preference->new )
      for @players;
    for my $n ( 0 .. $#players / 4 ) {
        $players[ 2 * $n ]->preference->direction($evenRole);
        $players[ 2 * $n ]->preference->difference(0);
        $players[ 2 * $n + 1 ]->preference->direction($oddRole);
        $players[ 2 * $n + 1 ]->preference->difference(0);
    }
    $self->entrants( \@players );
}


=head2 collectCards

 $play = $tourney->collectCards( @games );
  next if $htable->{$player1->id}->{$player2->id};

Records @games after they have been played. Stored as $tourney's play field, keyed on round and ids of players.  Returns the new play field. Updates player scores, preferences. TODO This has non-Swiss subclass elements I could factor out into a method in Games::Tournament. TODO What if player is matched more than one time in the round, filling in for someone? XXX It looks like all the games have to be the same round, or you have to collect all cards in one round before collecting cards in following rounds. XXX I'm having problems with recording roles. I want to be lazy about it, and trust the card I get back before the next round. The problem with this is, I may be getting the role from the wrong place. It should come from the card, and is a role which was assigned in the previous round, and is only now being recorded, at this point between the previous round and the next round. Or is the problem copying by value rather than reference of the entrants? Now I also need to record floats. It would be good to do this at the same time as I record roles. The card is the appropriate place to get this info according to A4.

=cut

sub collectCards {
    my $self     = shift;
    my @games    = @_;
    my $play     = $self->play || {};
    my @entrants = @{ $self->entrants };
    for my $entrant (@entrants) {
        my $id       = $entrant->id;
        my $oldroles = $entrant->roles;
        my $scores   = $entrant->scores;

        # my $pref = $entrant->preference;
        my @game = grep {
            grep { $id == $_->{id} } $_->myPlayers
        } @games;
        my $game  = $game[0];
        my $round = $game->round;
        my ( $role, $float );
        if ( $game and $game->isa("Games::Tournament::Card") ) {
	    # $game->canonize;
            $role             = $game->myRole($entrant);
            $float            = $game->myFloat($entrant);
            $scores->{$round} = $game->{result}->{$role};
            carp
"No result in round $round for player $id, $entrant->{name} as $role"
              unless $game->{result}->{$role};
            $play->{$round}->{$id} = $game || "No game";
        }
        else { warn "Player $id had no game in round $round"; $role = 'None'; }
        $entrant->scores($scores);
        die "No record in round $round for player $id $entrant->{name}"
          unless $play->{$round}->{$id};
        $entrant->roles($role);
        $entrant->floats( $round, $float );
        $entrant->floating('');
        $entrant->preference->update( $entrant->roles );
    }
    #die "Players without game near round $self->{round}"
    #    if grep {my $id = $_->id; not grep { $id eq $_->id } @clones }
    #    							    @entrants;

    $self->entrants( \@entrants );
    $self->play($play);
    # $self->updateScores;
    # $self->entrants(\@entrants);
}


=head2 publishCards

 $schedule = $tourney->publishCards( @games );

Announces @games before they have been played, and stores them as $tourney's play field, keyed on round and ids of players.  Returns the new play field. TODO This has non-Swiss subclass elements I could factor out into a method in Games::Tournament.

=cut

sub publishCards {
    my $self     = shift;
    my $play     = $self->play || {};
    my @entrants = @{ $self->entrants };
    my @games    = @_;
    for my $game (@games) {
        my $round       = $game->round;
        my $contestants = $game->contestants;
        my @players     = map { $contestants->{$_} } keys %$contestants;
        for my $player (@players) {
            my $id      = $player->id;
            my $entrant = $self->ided($id);
            die "Player $id $entrant in round $round?"
              unless $entrant
              and $entrant->isa("Games::Tournament::Contestant::Swiss");
            $play->{$round}->{$id} = $game;
        }
    }
    $self->play($play);
}


=head2 myCard

 $game = $tourney->myCard(round => 4, player => $alekhine);

Finds match from $tourney's play accessor, which is keyed on round and ids of players.

=cut

sub myCard {
    my $self    = shift;
    my %args    = @_;
    my $round   = $args{round};
    my $player  = $args{player};
    my $matches = $self->{play}->{$round};
    my $match   = grep { $_->{id} == $player->{id} } %$matches;
    return $match;
}


=head2 formBrackets

 @groups = $tourney->formBrackets

Returns for the next round an array of Games::Tournament::Swiss::Bracket objects grouping contestants with the same score, ordered by score. Late entrants without a score cause the program to die. Some groups may have odd numbers of players, etc, and players will have to be floated to other score groups.
TODO Should this have a round parameter, requiring score info up to that round?

=cut

sub formBrackets {
    my $self    = shift;
    my $players = $self->entrants;
    my %hashed;
    my @groups;
    foreach my $player (@$players) {
        my @group = ();
        my $score = defined $player->score ? $player->score : 0;

        # die "$player has no score. Give them a zero, perhaps?"
        #   if $score eq "None";
        $hashed{$score}{ $player->pairingNumber } = $player;
    }
    my $n = 0;
    foreach my $score ( reverse sort keys %hashed ) {
        my @members;
        foreach
          my $pairingNumber ( sort { $a <=> $b } keys %{ $hashed{$score} } )
        {
            push @members, $hashed{$score}{$pairingNumber};
        }
        use Games::Tournament::Swiss::Bracket;
        my $group = Games::Tournament::Swiss::Bracket->new(
            score   => $score,
            members => \@members
        );
        push @groups, $group;
    }

    # $self->brackets( \@groups );
    return @groups;
}

=head2 pairing

 $pairing = $tourney->pairing( \@groups );

Returns a Games::Tournament::Swiss::Procedure object. Groups are Games::Tournament::Swiss::Brackets objects of contestants with the same score and they are ordered by score, the group with the highest score first, and the group with the lowest score last. This is the point where round i becomes round i+1.

=cut

sub pairing {
    my $self     = shift;
    my $entrants = $self->entrants;
    my $brackets = shift;
    my $round    = $self->round;
    return Games::Tournament::Swiss::Procedure->new(
        round        => $round + 1,
        brackets     => $brackets,
        whoPlayedWho => $self->whoPlayedWho,
        colorClashes => $self->colorClashes,
        byes         => $self->byesGone,
    );
    # $self->{round} = $round+1;
}


=head2 compatible

	$games = $tourney->compatible
	next if $games->{$alekhine->pairingNumber}->{$capablanca->pairingNumber}

Returns an anonymous hash, keyed on the pairing numbers (ids) of @grandmasters, indicating whether or not the individual @grandmasters could play each other in the next round. But what is the next round? This method uses the whoPlayedWho and colorClashes methods to remove incompatible players.

=cut

sub compatible {
    my $self     = shift;
    my $players  = $self->entrants;
    my @ids      = map { $_->id } @$players;
    my $play     = $self->play;
    my $dupes    = $self->whoPlayedWho;
    my $colorbar = $self->colorClashes;
    my $compat;
    for my $id1 (@ids) {

        for my $id2 ( grep { $_ != $id1 } @ids ) {
            $compat->{$id1}->{$id2} = 1
              unless exists $dupes->{$id1}->{$id2}
              or exists $colorbar->{$id1}->{$id2};
        }
    }
    return $compat;
}


=head2 whoPlayedWho

	$games = $tourney->whoPlayedWho
	next if $games->{$alekhine->pairingNumber}->
	    {$capablanca->pairingNumber}

Returns an anonymous hash, keyed on the pairing numbers (ids) of the tourney's entrants, of the round in which individual entrants met. Don't forget to collect scorecards in the appropriate games first! (No tracking of how many times players have met if they have met more than once!) Do you know what round it is? B1

=cut

sub whoPlayedWho {
    my $self    = shift;
    my $players = $self->entrants;
    my @ids     = map { $_->id } @$players;
    my $play    = $self->play;
    my $dupes;
    my $lastround = $self->round;
    for my $round ( FIRSTROUND .. $lastround ) {
        for my $id (@ids) {
            my $player = $self->ided($id);
            die "No player with $id id in round $round game of @ids"
              unless $player;
            my $game = $play->{$round}->{$id};
            if ( $game and $game->can("myRole") ) {
                my $role = $game->myRole($player);
                die "Player $id, $player->{name}'s role is $role of " . ROLES
                  . " in round $round?"
                  unless grep { $_ eq $role } ROLES, 'Bye';
                my ( $otherRole, $opponent );
                if ( grep { $role eq $_ } ROLES ) {
                    $otherRole = ( grep { $role ne $_ } ROLES )[0];
                    $opponent = $game->contestants->{$otherRole};
                    $dupes->{$id}->{ $opponent->id } = $round;
                }
            }
            else { warn "Player ${id}'s game in round $round?"; }
        }
    }
    return $dupes;
}


=head2 colorClashes

	$nomatch = $tourney->colorClashes
	next if $nomatch->{$alekhine->id}->{$capablanca->id}

Returns an anonymous hash, keyed on the ids/pairing numbers of the tourney's entrants, of a color (role) if 2 of the individual @grandmasters both have an absolute preference for it in the next round and so can't play each other (themselves). Don't forget to collect scorecards in the appropriate games first! B2

=cut

sub colorClashes {
    my $self    = shift;
    my $players = $self->entrants;
    my @id      = map { $_->id } @$players;
    my $clashes;
    for my $player ( 0 .. $#$players ) {
        for ( 0 .. $#$players ) {
            $clashes->{ $id[$player] }->{ $id[$_] } =
              $players->[$player]->preference->role
              if $players->[$player]->preference->role
              and $players->[$_]->preference->role
              and $players->[$player]->preference->role eq
              $players->[$_]->preference->role
              and $players->[$player]->preference->strength eq 'Absolute'
              and $players->[$player]->preference->strength eq
              $players->[$_]->preference->strength;
        }
    }
    return $clashes;
}

=head2 byesGone

	next if $tourney->byesGone($grandmasters)

Returns an anonymous hash of either the round in which the tourney's entrants had a 'Bye' or the empty string, keyed on @$grandmasters' ids. If a grandmaster had more than one bye, the last one is returned. Don't forget to collect scorecards in the appropriate games first! B1

=cut


sub byesGone {
    my $self    = shift;
    my $players = $self->entrants;
    my @ids     = map { $_->id } @$players;
    my $play    = $self->play;
    my $byes;
    my $round = $self->round;
    for my $round ( FIRSTROUND .. $round ) {
        for my $id (@ids) {
            my $player = $self->ided($id);
            my $game   = $play->{$round}->{$id};
            if ( $game and $game->can("myRole") ) {
                eval { $game->myRole($player) };
                die "Role of player $id in round $round? $@"
                  if not $player or $@;
                my $role = $game->myRole($player);
                if ( $role eq 'Bye' ) {
                    $byes->{$id} = $round;
                }
            }
            else { warn "Player ${id}'s game in round $round?"; }
        }
    }
    return $byes;
}

=head2 incompatibles

	$nomatch = $tourney->incompatibles(@grandmasters)
	next if $nomatch->{$alekhine->id}->{$capablanca->id}

Collates information from the whoPlayedWho and colorClashes methods to show who cannot be matched or given a bye in the next round, returning an anonymous hash keyed on the ids/pairing numbers of @grandmasters. B1,2 C1,6

=cut

sub incompatibles {
    my $self              = shift;
    my $oldOpponents      = $self->whoPlayedWho;
    my $colorIncompatible = $self->colorClashes;
    my $players           = $self->entrants;
    my @id                = map { $_->id } @$players;
    my $unavailables;
    for my $player ( 0 .. $#$players ) {
        for ( 0 .. $#$players ) {
            my $color = $colorIncompatible->{ $id[$player] }->{ $id[$_] };
            my $round = $oldOpponents->{ $id[$player] }->{ $id[$_] };
            $unavailables->{ $id[$player] }->{ $id[$_] } = $color if $color;
            $unavailables->{ $id[$player] }->{ $id[$_] } ||= $round if $round;
        }
    }
    return $unavailables;
}


=head2 medianScore

 $group = $tourney->medianScore($round)

Returns the score equal to half the number of rounds that have been played. Half the contestants will have scores above or equal to this score and half will have ones equal to or below it, assuming everyone has played every round. What IS the number of rounds played, again?

=cut

sub medianScore {
    my $self  = shift;
    my $round = shift;
    return $round / 2;
}

=head2 rounds

	$tourney->rounds

Gets/sets the total number of rounds to be played in the competition

=cut

sub rounds {
    my $self   = shift;
    my $rounds = shift;
    if ( defined $rounds ) { $self->{rounds} = $rounds; }
    elsif ( $self->{rounds} ) { return $self->{rounds}; }
}


=head2 size

$size = 'Maxi' if $tourney->size > 2**$tourney->rounds

Gets the number of entrants

=cut

sub size {
    my $self = shift;
    return scalar @{ $self->entrants };
}

=head1 AUTHOR

Dr Bean, C<< <drbean, followed by the at mark (@), cpan, then a dot, and finally, org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-games-tournament-swiss at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Games-Tournament-Swiss>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Games::Tournament::Swiss

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Games-Tournament-Swiss>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Games-Tournament-Swiss>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Games-Tournament-Swiss>

=item * Search CPAN

L<http://search.cpan.org/dist/Games-Tournament-Swiss>

=back

=head1 ACKNOWLEDGEMENTS

See L<http://www.fide.com/official/handbook.asp?level=C04> for the FIDE's Swiss rules.

=head1 COPYRIGHT & LICENSE

Copyright 2006 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;    # End of Games::Tournament::Swiss

# vim: set ts=8 sts=4 sw=4 noet:
