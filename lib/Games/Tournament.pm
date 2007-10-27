package Games::Tournament;

# Last Edit: 2007 Oct 27, 11:29:23 AM
# $Id: $

use warnings;
use strict;

use Games::Tournament::Swiss::Config;
use constant ROLES      => @Games::Tournament::Swiss::Config::roles;
use constant FIRSTROUND => $Games::Tournament::Swiss::Config::firstround;

=head1 NAME

Games::Tournament - Contestant Pairing 

=head1 VERSION

Version 0.02

=cut

our $VERSION = '0.02';

=head1 SYNOPSIS

    $tourney = Games::Tournament->new(\@entrants);
    next if $capablanca->met($alekhine)

    $round = $tourney->meeting($member1, [$member2, $member3]);
    ...

=head1 DESCRIPTION

In a tournament, there are contestants, and matches over rounds between the contestants, in which they are differentiated by role. TODO firstround and roles.

=head1 METHODS

=head2 new

 Games::Tournament->new( rounds => 2, entrants => [ $a, $b, $c ] )

Creates a competition for entrants, over a number of rounds. entrants is a list of player objects.

=cut 

sub new {
    my $self = shift;
    my %args = @_;
    return bless \%args, $self;
}


=head2 rank

 @rankings = $tourney->rank(@players)

Ranks a list of Games::Tournament::Contestant player objects by score, rating, title and name if they all have a score, otherwise ranks them by rating, title and name.

=cut

sub rank {
    my $self    = shift;
    my @players = @_;
    unless ( grep { not defined $_->score } @players ) {
        sort {
                 $b->score <=> $a->score
              || $b->rating <=> $a->rating
              || $a->title cmp $b->title
              || $a->name cmp $b->name
        } @players;
    }
    else {
        sort {
                 $b->rating <=> $a->rating
              || $a->title cmp $b->title
              || $a->name cmp $b->name
        } @players;
    }
}

=head2 reverseRank

 @reverseRankings = $tourney->reverseRank(@players)

Ranks in reverse order a list of Games::Tournament::Contestant player objects by score, rating, title and name if they all have a score, otherwise reverseRanks them by rating, title and name.

=cut

sub reverseRank {
    my $self    = shift;
    my @players = @_;
    my @rankers = $self->rank(@players);
    return reverse @rankers;
}


#=head2 firstRound
#
#	$tourney->firstRound(7)
#
#Gets/sets the first round in the competition in which the swiss system is used to pair opponents, when this might not be the first round of the competition.
#
#=cut
#
#field 'firstRound' => 1;


=head2 named

    $tourney->named($name)

Returns the contestant whose name is $name. Entrants are grepped for the first one with a name with stringwise equality.

=cut 

sub named {
    my $self        = shift;
    my $name        = shift;
    my @contestants = @{ $self->entrants };
    return ( grep { $_->name eq $name } @contestants )[0];
}


=head2 ided

    $tourney->ided($id)

Returns the contestant whose id is $id. Ids are grepped for stringwise equality.

=cut 

sub ided {
    my $self        = shift;
    my $id          = shift;
    my @contestants = @{ $self->entrants };
    return ( grep { $_->id eq $id } @contestants )[0];
}


=head2 roleCheck

    roleCheck(@games)

Returns the roles of the contestants in the individual $games in @games, eg qw/Black White/, qw/Home Away/, these being all the same (ie no typos), or dies.

=cut 

sub roleCheck {
    my $self  = shift;
    my @games = @_;
    my @roles;
    for my $game (@games) {
        my $contestants = $game->contestants;
        my $result      = $game->result;
        my @otherroles  = sort keys %$contestants;
        for my $key ( keys %$result ) {
            die "$key: $result->{$key}, but no $key player in $game."
              unless grep { $key eq $_ } @otherroles;
        }
        unless (@roles) {
            @roles = @otherroles;
        }
        else {
            my $test = 0;
            $test++ unless @roles == @otherroles;
            for my $i ( 0 .. $#roles ) {
                $test++ unless $roles[$i] eq $otherroles[$i];
            }
            die "@roles in game 1, but @otherroles in $game."
              if $test;
        }
    }
    return @roles;
}


=head2 met

	@rounds = $tourney->met($deepblue, @grandmasters)
	next if $tourney->met($deepblue, $capablanca)

In list context, returns an array of the rounds in which $deepblue met the corresponding member of @grandmasters (and of the empty string '' if they haven't met.) In scalar context, returns the number of grandmasters met. Don't forget to collect scorecards in the appropriate games first! (Assumes players do not meet more than once!) This is same as Games::Tournament::Contestant::met or different?

=cut

sub met {
    my $self      = shift;
    my $player    = shift;
    my @opponents = @_;
    my @ids       = map { $_->id } @opponents;
    my $games     = $self->play;
    my $rounds    = $self->round;
    my @meetings;
    @meetings[ 0 .. $#opponents ] = ('') x @opponents;
    my $n = 0;
    for my $other (@opponents) {
        for my $round ( 1 .. $rounds ) {
            my $game = $games->{$round}->{ $other->id };
            $meetings[$n] = $round if $other->myOpponent($game) == $player;
        }
    }
    continue { $n++; }
    return @meetings if wantarray;
    return scalar grep { $_ } @meetings;
}


=head2 unmarkedCards

	@unfinished = $tourney->unmarkedCards(@games)

Returns an array of the games which have no or a wrong result. The result accessor should be an anonymous hash with roles, or 'Bye' as keys and either 'Win' & 'Loss', 'Loss' & 'Win' or 'Draw' & 'Draw', or 'Bye', as values.

=cut

sub unmarkedCards {
    my $self  = shift;
    my @games = @_;
    my @unfinished;
    for my $game (@games) {
        my $contestants = $game->contestants;
        my $result      = $game->result;
        push @unfinished, $game
          unless (
            ( keys %$contestants == 1 and $result->{Bye} =~ m/Bye/i )
            or $result->{ (ROLES)[0] } and $result->{ (ROLES)[1] }
            and (
                (
                        $result->{ (ROLES)[0] } eq 'Win'
                    and $result->{ (ROLES)[1] } eq 'Loss'
                )
                or (    $result->{ (ROLES)[0] } eq 'Loss'
                    and $result->{ (ROLES)[1] } eq 'Win' )
                or (    $result->{ (ROLES)[0] } eq 'Draw'
                    and $result->{ (ROLES)[1] } eq 'Draw' )
            )
          );
    }
    return @unfinished;
}


=head2 dupes

	$games = $tourney->dupes(@grandmasters)

Returns an anonymous array, of the games in which @grandmasters have met. Don't forget to collect scorecards in the appropriate games first! (Assumes players do not meet more than once!)

=cut

sub dupes {
    my $self    = shift;
    my @players = @_;
    my @ids     = map { $_->id } @players;
    my $games   = $self->play;
    my @dupes;
    map {
        my $id = $_;
        map { push @dupes, $games->{$id}->{$_} if exists $games->{$id}->{$_}; }
          @ids;
    } @ids;
    return \@dupes;
}


=head2 updateScores

 @scores = $tourney->updateScores;

Updates entrants' scores for the present (previous) round, using $tourney's play (ie games played) field. Returns an array of the scores in order of the player ids (not at the moment, it doesn't), dying on those entrants who don't have a result for the round. Be careful. Garbage in, garbage out. What is the present round?

=cut

sub updateScores {
    my $self    = shift;
    my $players = $self->entrants;
    my $round   = $self->round;
    my $games   = $self->play;
    my @scores;
    for my $player (@$players) {
        my $id     = $player->id;
        my $oldId  = $player->oldId;
        my $scores = $player->scores;
        my $card   = $games->{$round}->{$id};
        die "Game in round $round for player $id? Is $round the right round?"
          unless $card
          and $card->isa('Games::Tournament::Card');
        my $results = $card->{result};
        die @{ [ keys %$results ] } . " roles in player ${id}'s game?"
          unless grep { $_ eq (ROLES)[0] or $_ eq (ROLES)[1] or $_ eq 'Bye' }
          keys %$results;
        eval { $card->myResult($player) };
        die "$@: Result in player ${id}'s $card game in round $round?"
          if not $card or $@;
        my $result = $card->myResult($player);
        die "$result result in $card game for player $id in round $round?"
          unless $result =~ m/^(?:Win|Loss|Draw|Bye|Absent)/i;
        $$scores{$round} = $result;
        $player->scores($scores) if defined $scores;
        push @scores, $$scores{$round};
    }
    $self->entrants($players);
    # return @scores;
}


#=head2 round
#
# $group = $tourney->round
#
#Returns the present round, ie the first round for which one or more players does not have a result, minus 1. The conservative approach.
#
#=cut
#
#sub round {
#    # my $self = shift;
#    my $players = $self->entrants;
#    my $rounds  = $self->rounds;
#    my $play    = $self->play;
#    my $round   = min map {
#        my $n = 1;
#        $n++ while exists $play->{$n}
#          and $play->{$n}->{ $_->id };
#        $n - 1;
#    } @$players;
#    return $round;
#}


=head2 randomRole

 ( $myrole, $yourrole ) = randomRole;

This returns the 2 roles, @Games::Tournament::roles in a random order.

=cut

sub randomRole {
    my $self     = shift;
    my $evenRole = int rand(2) ? (ROLES)[0] : (ROLES)[1];
    my $oddRole  = $evenRole eq (ROLES)[0] ? (ROLES)[1] : (ROLES)[0];
    return ( $evenRole, $oddRole );
}


=head2 play

	$tourney->play

Gets the games played, keyed on round and id of player. Also sets, but you don't want to do that.

=cut

sub play {
    my $self = shift;
    my $play = shift;
    if ( defined $play ) { $self->{play} = $play; }
    elsif ( $self->{play} ) { return $self->{play}; }
}

=head2 entrants

	$tourney->entrants

Gets/sets the entrants as an anonymous array of player objects.

=cut

sub entrants {
    my $self     = shift;
    my $entrants = shift;
    if ( defined $entrants ) { $self->{entrants} = $entrants; }
    elsif ( $self->{entrants} ) { return $self->{entrants}; }
}


=head2 round

	$tourney->round

Gets/sets the round number of a round near you.

=cut

sub round {
    my $self  = shift;
    my $round = shift;
    if ( defined $round ) { $self->{round} = $round; }
    elsif ( $self->{round} ) { return $self->{round}; }
}


=head2 rounds

	$tourney->rounds

Gets/sets the number of rounds in the tournament.

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

=head2 odd

 float($lowest) if $self->odd(@group)

Tests whether the number of players in @group is odd or not.

=cut

sub odd {
    my $self = shift;
    my @n    = @_;
    return @n % 2;
}

=head1 AUTHOR

Dr Bean, C<< <drbean, followed by the at mark (@), cpan, then a dot, and finally, org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-games-tournament-swiss at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Games-Tournament>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Games::Tournament

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Games-Tournament>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Games-Tournament>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Games-Tournament>

=item * Search CPAN

L<http://search.cpan.org/dist/Games-Tournament>

=back

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2006 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;    # End of Games::Tournament

# vim: set ts=8 sts=4 sw=4 noet:
