package Games::Tournament::Contestant;

# Last Edit: 2007 Oct 27, 11:29:58 AM
# $Id: $

use warnings;
use strict;

use base qw/Games::Tournament/;
use List::Util qw/sum/;
use List::MoreUtils qw/all/;
use constant SCORES => %Games::Tournament::Swiss::Config::scores;

# use overload qw/0+/ => 'id', qw/""/ => 'name', fallback => 1;

=head1 NAME

Games::Tournament::Contestant  A competitor matched with others over a series of rounds

=head1 VERSION

Version 0.03

=cut

our $VERSION = '0.03';

=head1 SYNOPSIS

    my $foo = Games::Tournament::Contestant->new( rating => '15', name => 'Your New Knicks' );
    ...

=head1 DESCRIPTION

A generic tournament/series player/team contestant object.

=head1 METHODS

=head2 new

	$team = Games::Tournament::Contestant->new( id => '15', name => 'Lala Lakers', rating => 0, score => 1000,  )
	$grandmaster = Games::Tournament::Contestant->new( name => 'Jose Raul Capablanca', rating => 1000 )

=cut

sub new {
    my $self = shift;
    my %args = @_;
    return bless \%args, $self;
}


=head2 clone

	$monster = $alekhine->clone( score => 1000, reputation => 'bad' )

Creates a similar object to $alekhine, with the same id, name, score, title, and rating fields but with any other changes or additions you want to make.

=cut

sub clone {
    my $self  = shift;
    my %args  = @_;
    my $clone = Games::Tournament::Contestant->new(
        id     => $self->id     || undef,
        name   => $self->name   || undef,
        score  => $self->score  || undef,
        title  => $self->title  || undef,
        rating => $self->rating || undef,
    );
    foreach my $key ( keys %args ) {
        $clone->{$key} = $args{$key};
    }
    return $clone;
}


=head2 findCard

 @venues = $player->findCard(@games);

Returns a/the first game in @games in which $player is a contestant. 'findCard' expects the game objects to have 'contestants' accessors and be 'canonize'able. The players are grepped for stringwise id equality.

=cut

sub findCard {
    my $self  = shift;
    my $id    = $self->id;
    my @games = @_;
    my @cards;
    foreach my $game (@games) {
        $game->canonize;
        my $players = $game->contestants;
        push @cards, $game
          if grep { $players->{$_}->id eq $id } keys %$players;
    }
    return $cards[0];
}


=head2 myOpponent

 $opponent = $player->myOpponent($game);

Returns a/the opponent in $game of $player. 'myOpponent' expects the game object to have 'contestants' accessors. The players are grepped for stringwise id equality.

=cut

sub myOpponent {
    my $self        = shift;
    my $id          = $self->id;
    my $game        = shift;
    my $contestants = $game->contestants;
    my @contestants = values %$contestants;
    my @ids         = map { $_->id } @contestants;
    die "Player $id not in match of @ids" unless grep m/$_/, @ids;
    my @opponents;

    for my $contestant (@contestants) {
        push @opponents, $contestant if $contestant->id ne $id;
    }
    return $opponents[0];
}


=head2 copyCard

 @result = $player->copyCard(@games);

Stores a ref to the @games in which $player has participated and copied the cards for. @games may or may not be a complete list of result for all rounds, and may include games in which $player wasn't a participant. Pushed to an anonymous array stored as the 'play' field. 'copyCard' expects the game objects to have 'round' and 'contestants' accessors and be 'canonize'able.

=cut

sub copyCard {
    my $self  = shift;
    my $id    = $self->id;
    my $play  = $self->play;
    my @games = @_;
    my %result;
    foreach my $game (@games) {
        $game->canonize;
        my $round   = $game->round;
        my $players = $game->contestants;
        my %roles   = map { $players->{$_}->id => $_ } keys %$players;
        next unless exists $roles{$id};
        push @$play, $game;
    }
    $self->play($play);
}

=head2 writeCard (deprecated)

 @result = $player->writeCard(@games);

Updates the contestant's result in the matches played, using no intelligence if records only have only opponents' scores. @games may or may not be a complete list of result for all rounds, and may include games in which $player wasn't a participant. Stored as a 'play' field and keyed on the round, the resultant records have 'opponent' and 'result' subfields. 'writeCard' expects the game objects to have 'round', 'contestants' and 'result' accessors. Returns the new play field.
TODO The 'opponent' subfield will be an anonymous array of player objects if it is a multi-player game.

=cut

sub writeCard {
    my $self  = shift;
    my $id    = $self->id;
    my @games = @_;
    my %result;
    foreach my $game (@games) {
        $game->canonize;
        my $round   = $game->round;
        my $players = $game->contestants;
        my %roles   = map { $players->{$_}->id => $_ } keys %$players;
        next unless exists $roles{$id};
        my $role = $roles{$id};
        my $opponent;
        foreach my $player ( values %$players ) {
            $opponent = $player unless $player->id == $self->id;
        }
        $result{$round} = { opponent => $opponent };
        $result{$round}{result} = $game->{result}->{$role};
    }
    $self->play( \%result );
}


=head2 score

	$rounds = $deepblue->score
	next if $deepblue->score

Gets/sets the total score over the rounds in which $deepblue has a score. Don't forget to tally $deepblue's scorecard with the appropriate games first! We don't check any cards. Internally, this method accumulates the results of all the rounds into a total score, unless no results exist. If they don't exist, a hash key $self->{score} is consulted. You can set the score this way too, bypassing the elegant code to do it from individual game results stored by the Games::Tournament::Contestant object. A hack to allow importing a pairing table.

=cut

sub score {
    my $self        = shift;
    my %converter   = SCORES;
    my $score = shift;
    if ( defined $score ) { $self->{score} = $score; }
    my $scores      = $self->scores;
    return $self->{score} || 0 unless defined $scores and
		all { defined $_ } values %$scores;
    my %lcconverter = map { lc($_) => $converter{$_} } keys %converter;
    my %scores      = map { $_ => lc $scores->{$_} } keys %$scores;
    for my $round ( keys %scores ) {
        die
"Round $round $scores->{$round}, $scores{$round} score unconvertible to $lcconverter{$scores{$round}} for player $self->{id}"
          unless defined( $scores{$round} and $lcconverter{ $scores{$round} } );
    }
    my @values = map { $lcconverter{$_} } values %scores;
    my $sum = sum(@values);
    return $sum if defined $sum;
    return 0;
}


=head2 met

	$rounds = $deepblue->met(@grandmasters)
	next if $deepblue->met($capablanca)

Returns an anonymous array either of the rounds in which $deepblue remembers meeting the members of @grandmasters or of the empty string '' if there is no record of such a meeting. Don't forget to tally $deepblue's scorecard with the appropriate games first! We don't check $deepblue's partners' cards. (Assumes players do not meet more than once!)
This is same as Games::Tournament::met or different?

=cut

sub met {
    my $self      = shift;
    my @opponents = @_;
    my $games     = $self->play;

    # my $id = $partner->id;
    # my %opponentsMet;
    # @opponentsMet{ keys %$games } = ('') x keys %$games;
    # map { $opponentsMet{$games->{$_}->{opponent}->id} = $_} keys %$games;
    # return $opponentsMet{$id};
    my @ids = map { $_->id } @opponents;
    my %opponentsMet;
    @opponentsMet{@ids} = ('') x @ids;
    for my $game ( keys %$games ) {
        $opponentsMet{ $games->{$game}->{opponent}->id } = $_;
    }
    my @opponentsMet = map { $opponentsMet{ $_->id } } @opponents;
    return \@opponentsMet;
}


=head2 name

	$member->name('Alexander Alekhine');
	$member->name

Sets or gets the name of the contesting individual or team, a string that may or may not be unique to the tournament member.

=cut

sub name {
    my $self = shift;
    my $name = shift;
    if ( defined $name ) { $self->{name} = $name; }
    elsif ( $self->{name} ) { return $self->{name}; }
}


=head2 title

	$member->title('Grandmaster')

Sets/gets the title of the contestant, a courtesy given to the contestant.

=cut

sub title {
    my $self  = shift;
    my $title = shift;
    if ( defined $title ) { $self->{title} = $title; }
    elsif ( $self->{title} ) { return $self->{title}; }
}


=head2 scores

	$member->scores

Sets/gets the scores (actually results, eg 'Draw', 'Win') of the contestant in the different matches of the tournament, an ongoing record of their standing in the competition. These scores may or may not include the current score. To calculate the total score, use 'score', because internally the scores are not stored as number scores.

=cut

sub scores {
    my $self = shift;
    my $scores = shift() || $self->{scores};
    $self->{scores} = $scores;
    return $scores;
}


=head2 rating

	$member->rating

Sets/gets the rating of the contestant, an estimate of their strength. The constructor assumes if no rating or a non-numeric rating is given, that they don't have a rating, and it is set to 0.

=cut

sub rating {
    my $self   = shift;
    my $rating = shift;
    if ( defined $rating and $rating =~ m/^\d$/ ) { $self->{rating} = $rating; }
    elsif ( $self->{rating} ) { return $self->{rating}; }
    else { return 0; }
}


=head2 play

	$games = $member->play;
	$games = $member->play(
		{ 1 => { opponent => $grandmaster, result => 'Loss' } });

Sets/gets a hash reference to the result of the pairings in each of the rounds played so far. Don't use this to enter a player's match result. Use $p->writeCard instead. Implementation: The keys of the hash are the round numbers and the values are themselves hash references, to the other player and whether the result was a 'Win', 'Loss', or 'Draw', with as keys, 'opponent' and 'result', NO. I think it is the gamecard of the player in that round. Or is that the play accessor for tournaments?

=cut

sub play {
    my $self = shift;
    my $play = shift;
    if ( defined $play ) { $self->{play} = $play; }
    elsif ( $self->{play} ) { return $self->{play}; }
}


=head2 id

	$member->id

Returns/sets the id of the contestant, a number unique to the member.

=cut

sub id {
    my $self = shift;
    my $id   = shift;
    if ( defined $id ) { $self->{id} = $id; }
    elsif ( exists $self->{id} ) { return $self->{id}; }
}

=head1 AUTHOR

Dr Bean, C<< <drbean, followed by the at mark (@), cpan, then a dot, and finally, org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-games-tournament-contestant at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Games-Tournament-Swiss>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Games::Tournament::Contestant

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

=head1 COPYRIGHT & LICENSE

Copyright 2006 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;    # End of Games::Tournament::Contestant

# vim: set ts=8 sts=4 sw=4 noet:
