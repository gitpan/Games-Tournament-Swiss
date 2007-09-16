package Games::Tournament::Swiss::Bracket;

# Last Edit: 2007 Sep 14, 03:24:55 PM
# $Id: $

use warnings;
use strict;

use constant ROLES => @Games::Tournament::Swiss::Config::roles;

use base qw/Games::Tournament::Swiss/;
use Games::Tournament::Contestant::Swiss;
use Games::Tournament::Card;
use List::Util qw/max min reduce sum/;

=head1 NAME

Games::Tournament::Swiss::Bracket - Players with same/similar scores pairable with each other

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

    $tourney = Games::Tournament::Swiss>new($rounds, \@entrants);
    @rankedPlayers = $tourney->assignPairingNumbers;
    @firstbrackets = $t->formBrackets;
    ...
    $tourney->collectCards(@games);
    @scores = $tourney->updateScores($round);
    @groups = $tourney->formBrackets;

=head1 DESCRIPTION

In a Swiss tournament, in each round contestants are paired with other players with the same, or similar, scores. These contestants are grouped into a score group (bracket) in the process of deciding who plays who.

The concept of immigration control is applied to impose order on the players floating in and out of these score brackets. That is, floating is like flying.

=head1 METHODS

=head2 new

 $group = Games::Tournament::Swiss::Bracket->new( score => 7.5, members => [ $a, $b, $c ], remainderof => $largergroup )

members is a reference to a list of Games::Tournament::Contestant::Swiss objects. The order is important. If the score group includes floaters, these members' scores will not be the same as $group->score. Such a heterogenous group is paired in two parts--first the downfloaters, and then the homogeneous remainder group. Remainder groups can be recognized by the existence of a 'remainderof' key that links them to the group they came from. Some members may also float down from a remainder group. A3

=cut 

sub new {
    my $self = shift;
    my %args = @_;
    $args{criterion} = { B5 => 'Up&Down', B6 => 'Up&Down' };
    bless \%args, $self;
    return \%args;
}


=head2 natives

 @floaters = $group->natives

Returns those members who were in this bracket originally, as that was their birthright, their scores being all the same. One is a native of only one bracket, and you cannot change this status except by naturalization.

=cut

sub natives {
    my $self = shift;
    return () unless @{ $self->members };
    my $members    = $self->members;
    my $foreigners = $self->immigrants;
    my @natives    = grep {
        my $member = $_->{id};
        not grep { $member == $_->{id} } @$foreigners
    } @$members;
    return \@natives;
}


=head2 naturalize

 $citizen = $group->naturalize($foreigner)

Gives members who are resident, but not natives, ie immigrants, having been floated here from other brackets, the same status as natives, making them indistinguishable from them. This will fail if the player is not resident or not an immigrant. Returns the player with their new status.

=cut

sub naturalize {
    my $self      = shift;
    my $foreigner = shift;
    my $members   = $self->residents;
    return unless grep { $_->id == $foreigner->id } @$members;
    my $direction = $foreigner->floating;
    return unless $direction eq 'Up' or $direction eq 'Down';
    $foreigner->floating('');
    return $foreigner;
}


=head2 immigrants

 @floaters = @{$group->immigrants}

Returns those members who are foreigners, having been floated here from other brackets. At any one point a player may or may not be a foreigner. But if they are, they only can be a foreigner in one bracket. 

=cut

sub immigrants {
    my $self = shift;
    return () unless @{ $self->members };
    my $members = $self->residents;
    my @immigrants = grep { $_->floating } @$members;
    return \@immigrants;
}


=head2 downFloaters

 @floaters = $group->downFloaters

Returns those members downfloated here from the previous bracket.

=cut

sub downFloaters {
    my $self = shift;
    return () unless @{ $self->members };
    my $floaters = $self->immigrants;
    grep { $_->floating and $_->floating =~ m/^Down/i } @$floaters;
}


=head2 upFloaters

 @s1 = $group->upFloaters

Returns those members upfloated from the next bracket.

=cut

sub upFloaters {
    my $self = shift;
    return () unless @{ $self->members };
    my @members = $self->residents;
    grep { $_->floating and $_->floating =~ m/^Up/i } @{ $self->members };
}


=head2 residents

	$pairables = $bracket->residents

Returns the members includeable in pairing procedures for this bracket because they haven't been floated out, or because they have been floated in. That is, they are not an emigrant. At any one point, a player is resident in one and only one bracket, unless they are in transit. At some other point, they may be a resident of another bracket.

=cut

sub residents {
    my $self    = shift;
    my @members = @{ $self->{members} };
    my @residents;
    my $floated = $self->emigrants;
    for my $member (@members) {
        push @residents, $member
          unless grep { $member->{id} == $_->{id} } @$floated;
    }
    return \@residents;
}


=head2 emigrants

	$bracket->emigrants($member)
	$gone = $bracket->emigrants

Sets whether this native member will not be included in pairing of this bracket. That is whether they have been floated to another bracket for pairing there. Gets all such members. A player may or may not be an emigrant. They can only stop being an emigrant if they move back to their native bracket. To do this, they have to return.

=cut

sub emigrants {
    my $self    = shift;
    my $floater = shift;
    if ($floater) { push @{ $self->{gone} }, $floater; }
    else { return $self->{gone}; }
}


=head2 exit

	$bracket->exit($player)

Removes $player from the list of members if $player is an immigrant. If a native, adds them to the list of emigrants of this bracket. They are now in limbo. So make sure they enter another bracket.

=cut

sub exit {
    my $self       = shift;
    my $member     = shift;
    my $members    = $self->members;
    my $immigrants = $self->immigrants;
    if ( grep { $_ == $member } @$immigrants ) {
        @{ $self->members } = grep { $_ != $member } @$members;
    }
    else {
        $self->emigrants($member);
    }
    return;
}


=head2 entry

	$bracket->entry($native)
	$bracket->entry($foreigner)

Registers $foreigner as a resident, and removes $native from the list of emigrants of this bracket, because they have returned from another bracket as in C12, 13.

=cut

sub entry {
    my $self   = shift;
    my $member = shift;
    die
"Player $member->{id} floating $member->{floater} from $self->{score}-score bracket?"
      unless $member->floating
      and $member->floating =~ m/Down|Up/;
    my $members   = $self->members;
    my $emigrants = $self->emigrants;
    if ( grep { $_ == $member } @$emigrants ) {
        $self->reentry($member);
    }
    else {
        if ( $member->floating eq 'Down' ) { unshift @$members, $member; }
        else { push @$members, $member; }
        $self->members($members);
    }
    return;
}


=head2 reentry

	$bracket->reentry($member)

Removes this native (presumably) member from the list of emigrants of this bracket, because they have returned from another bracket as in C12, 13. Returns undef, if $member wasn't an emigrant. Otherwise returns the updated list of emigrants.

=cut

sub reentry {
    my $self      = shift;
    my $returnee  = shift;
    my $emigrants = $self->emigrants;
    if ( grep { $_ == $returnee } @$emigrants ) {
        my @nonreturnees = grep { $_ != $returnee } @$emigrants;
        @{ $self->{gone} } = @nonreturnees;
        return @nonreturnees;
    }
    return;
}


=head2 s1

 $group->s1
 $s1 = $group->s1($players)
 $s1 = $group->s1

Getter/setter of the p players in the top half of a homogeneous bracket, or the p downFloaters in a heterogeneous bracket, as an array. A6

=cut

sub s1 {
    my $self = shift;
    my $s1   = shift;
    if ( defined $s1 ) {
        $self->{s1} = $s1;
        return $s1;
    }
    elsif ( $self->{s1} ) { return $self->{s1}; }
    else { $self->resetS12; return $self->{s1}; }
}


=head2 s2

 $s2 = $group->s2

Getter/Setter of the players in a homogeneous or a heterogeneous bracket who aren't in S1. A6

=cut

sub s2 {
    my $self = shift;
    my $s2   = shift;
    if ( defined $s2 ) {
        $self->{s2} = $s2;
        return $s2;
    }
    elsif ( $self->{s2} ) { return $self->{s2}; }
    else { $self->resetS12; return $self->{s2}; }
}


=head2 resetS12

 $group->resetS12

Resetter of S1 and S2 to the original members, ranked before exchanges in C8. A6

=cut

sub resetS12 {
    my $self    = shift;
    my $members = $self->residents;
    return [] unless $#$members >= 1;
    my @downfloaters = $self->downFloaters;
    my (@s1, @s2);
    use Games::Tournament;
    if ( @downfloaters and $self->hetero ) {
        @s1 = @downfloaters;
	my %downfloaters = map { $_->id => $_ } @downfloaters;
	@s2 = grep { not exists $downfloaters{$_->id} } $self->rank(@$members);
    }
    else {
        my $p       = $self->p;
        @s1 = ( $self->rank(@$members) )[ 0 .. $p - 1 ];
        @s2 = ( $self->rank(@$members) )[ $p .. $#$members ];
    }
    $self->{s1} = \@s1;
    $self->{s2} = \@s2;
    return;
}


=head2 p

 $tables = $group->p

Half the number of players in a homogeneous bracket, rounded down to the next lowest integer. Or the number of down floaters in a heterogeneous bracket. Also the number of players in S1, and thus the number of pairings in the pair group. (See A1,2)A6

=cut

sub p {
    my $self    = shift;
    my $members = $self->residents;
    return 0 unless $#$members >= 1;
    my @downfloaters = $self->downFloaters;
    my $p;
    if ( @downfloaters and $self->hetero ) {
        $p = @downfloaters;
    }
    else {
        my $n = $#$members + 1;
        $p = int( $n / 2 );
    }
}


=head2 pprime

 $tables = $group->pprime

p is half the number of players in a bracket, but we may have to accept fewer pairings than this number if suitable opponents cannot be found for players, up to the point where p=0. pprime sets/gets this real p number. A8

=cut

sub pprime {
    my ( $self, $p ) = @_;
    my $pprime = $self->{pprime};
    if ( defined $p ) { $self->{pprime} = $p; }
    elsif ( defined $pprime ) { return $pprime; }
    else {
        $self->{pprime} = $self->p;
        return $self->{pprime};
    }
}


=head2 q

 $tables = $group->q

Number of players in the score bracket divided by 2 and then rounded up. In a homogeneous group with an even number of players, this is the same as p. A8

=cut

sub q {
    my $self    = shift;
    my $players = $self->residents;
    my $q = @$players % 2 ? ( $#$players + 2 ) / 2 : ( $#$players + 1 ) / 2;
}


=head2 x

 $tables = $group->x

Sets the number, ranging from zero to p, of matches in the score bracket in which players will have their preferences unsatisfied. A8

=cut

sub x {
    my $self    = shift;
    my $players = $self->residents;
    my $w       =
      grep { $_->preference->role and $_->preference->role eq (ROLES)[0] }
      @$players;
    my $b = @$players - $w;
    my $q = $self->q;
    my $x = $w >= $b ? $w - $q : $b - $q;
    $self->{x} = $x;
}


=head2 xprime

 $tables = $group->xprime

x is the lower limit on matches where preferences are not satisfied, but the number of such undesirable matches may be increased if suitable opponents cannot be found for players, up to the point where only players with Absolute preferences have their preferences satisfied. xprime sets/gets this real x number. A8

=cut

sub xprime {
    my $self   = shift;
    my $x      = shift;
    my $xprime = $self->{xprime};
    if ( defined $x ) { $self->{xprime} = $x; }
    elsif ( defined $xprime ) { return $xprime; }
    else {
        $self->{xprime} = $self->x;
        return $self->{xprime};
    }
}


=head2 hetero

	$group->hetero

Gets (but doesn't set) whether this group is heterogeneous, ie includes players who have been downfloated from a higher score group, or upfloated from a lower score group, or if it is homogeneous, ie every player has the same score. A group where half or more of the members have come from a higher bracket is regarded as homogeneous. We use the scores of the players, rather than a floating flag.

=cut

sub hetero {
    my $self = shift;
    my @members = @{$self->members};
    my %tally;
    %tally = map { my $score=$_->score; $score,++$tally{$score} } @members;
    my @range = keys %tally;
    return 0 if @range == 1;
    my $min = min @range;
    return 0 if $tally{$min} <= @members/2;
    return 1 if $tally{$min} > @members/2;
    return;
}


=head2 c7shuffler

	$nextS2 = $bracket->c7shuffler($firstmismatch)
	if ( @nextS2 compatible )
	{
	    create match cards;
	}

Gets the next permutation of the second-half players in D1 transposition counting order, as used in C7, that will not have the same incompatible player in the bad position found in the present transposition. If you get an illegal modulus error, check your $firstmismatch is a possible value.

=cut

sub c7shuffler {
    my $self     = shift;
    my $position = shift;
    my $bigLastGroup = shift;
    my $s2       = $self->s2;
    die "pos $position past end of S2" if $position > $#$s2;
    my @players  = $self->rank(@$s2);
    @players  = $self->reverseRank(@$s2) if $bigLastGroup;
    # my @players  = @$s2;
    my $p        = $self->p;
    my @pattern;
    my @playerCopy = @players;
    for my $i ( 0 .. $#$s2 ) {
        my $j = 0;
        $j++ until $s2->[$i]->{id} == $playerCopy[$j]->{id};
        $pattern[$i] = $j;
        splice @playerCopy, $j, 1;
    }
    my $value = $pattern[$position];
    my @nextPattern;
    @nextPattern[ 0 .. $position ] = @pattern[ 0 .. $position ];
    @nextPattern[ $position + 1 .. $#pattern ] =
      (0) x ( $#pattern - $position );
    for my $digit ( reverse( 0 .. $position ) ) {
	die "${digit}th digit overrun of @pattern \@pattern" if
						    @pattern == $digit;
        $nextPattern[$digit] = ++$value % ( @pattern - $digit );
        last unless $nextPattern[$digit] == 0;
    }
    continue { $value = $pattern[ $digit - 1 ]; }
    return unless grep { $_ } @nextPattern;
    my @permutation;
    for my $pos (@nextPattern) {
        push @permutation, splice( @players, $pos, 1 );
    }
    return @permutation;

 #my @selectS2 = $group->c7shuffler($badpair);
 #my @unselectS2  = @$s2;
 #for my $position ( 0 .. $#$s2 )
 #{
 #    my $player = $s2->[$#$s2 - $position];
 #    splice @unselectS2, $#$s2 - $position, 1 if grep{$_ eq $player} @selectS2;
 #}
 #my @newS2 = (@selectS2, @unselectS2);
}


=head2 c7iterator

	$next = $bracket->c7iterator
	while ( my @s2 = &$next )
	{
	    create match cards unless this permutation is incompatible;
	}

DEPRECATED Creates an iterator for the permutation of the second-half players in D1 transposition counting order, as used in C7. Only as many players as are in S1 can be matched, so we get only the permutations of all the p-length combinations of members of S2. Deprecated because if C1 or C6 finds a player in a certain position in S2 should not be paired with the player in the corresponding position in S1, we need to be able to skip ahead to the next permutation where a different player is in that position.

=cut 

sub c7iterator {
    my $self    = shift;
    my $players = $self->s2;
    my $p       = $self->p;
    my $n       = 0;
    return sub {
        my @pattern = n_to_pat->( $n, $#$players + 1, $p );
        my @result = permGenerator->( \@pattern, $players );
        print "transposition $n:\t";
        $n++;
        return @result;
    };
    my $permGenerator = sub {
        my $pattern = shift;
        my @items   = @{ shift() };
        my @r;
        for my $pos (@$pattern) {
            push @r, splice( @items, $pos, 1 );
        }
        return @r;
    };
    my $n_to_pat = sub {
        my @odometer;
        my ( $n, $length, $k ) = @_;
        for my $i ( $length - $k + 1 .. $length ) {
            unshift @odometer, $n % $i;
            $n = int( $n / $i );
        }
        return $n ? () : @odometer;
    };
}


=head2 c8iterator

	$next = $bracket->c8iterator
	while ( my @members = &$next )
	{
	    next if grep {$incompat{$s1[$_]}{$s2[$_]}} 0..$p-1);
	}

Creates an iterator for the exchange of @s1 and @s2 players in D2 order, as used in C8. Exchanges are performed in order of the difference between the pairing numbers of the players exchanged. If the difference is equal, the exchange with the lowest player is to be performed first. XXX Only as many players as in S1 can be matched, so does this mean some exchanges don't have an effect? I don't understand the description when there are an odd number of players. There appears to be a bug with only 3 players. 1 and 2 should be swapped, I think. I think the order of exchanges of 2 players each may also have some small inconsistencies with the FIDE order.

=cut 

sub c8iterator {
    my $self      = shift;
    my $n         = 0;
    my $p         = $self->p;
    my @exchanges = map {
        my $i = $_;
        map { [ [ $_, $_+$i ] ] }
          reverse( ( max 1, $p-$i ) .. ( min $p-1, 2*($p-1)-$i ) )
    } ( 1 .. 2*($p-1)-1 );
    my @s1pair = map {
        my $i = $_;
        map { [ $i - $_, $i ] } 1 .. $i - 1
    } reverse 2 .. $p - 1;
    my @s2pair = map {
        my $i = $_;
        map { [ $i, $i + $_ ] } 1 .. 2 * ( $p - 1 ) - $i
    } $p .. 2 * ( $p - 1 ) - 1;
    my @exchanges2 = map {
        my $i = $_;
        map {
            [
                [ $s1pair[$_][0], $s2pair[ $i - $_ ][0] ],
                [ $s1pair[$_][1], $s2pair[ $i - $_ ][1] ]
            ]
          } ( max 0, $i - ( $p - 1 ) * ( $p - 2 ) / 2 + 1 )
          .. ( min( ( $p - 1 ) * ( $p - 2 ) / 2 - 1, $i ) )
    } 0 .. ( $p - 1 ) * ( $p - 2 ) - 2;
    push @exchanges, @exchanges2;
    return sub {
        print "exchange " . ($n+1) . ":\t";
        return () unless $exchanges[$n];
	# my @members = @{ $self->members };
	$self->resetS12;
	my $s1 = $self->s1;
	my $s2 = $self->s2;
	my @members = (@$s1, @$s2);
        ( $members[ $_->[0] ], $members[ $_->[1] ] ) =
          ( $members[ $_->[1] ], $members[ $_->[0] ] )
          for @{ $exchanges[$n] };
        $n++;
        return @members;
      }
}


=head2 score

	$group->score

Gets/sets the score of the score group.

=cut

sub score {
    my $self  = shift;
    my $score = shift;
    if ( defined $score ) { $self->{score} = $score; }
    elsif ( exists $self->{score} ) { return $self->{score}; }
}


=head2 members

	$group->members

Gets/sets the members of the score group as an anonymous array of player objects. The order of this array is important. The first half is paired with the second half.

=cut

sub members {
    my $self    = shift;
    my $members = shift;
    if ( defined $members ) { $self->{members} = $members; }
    elsif ( $self->{members} ) { return $self->{members}; }
}


=head2 c8swapper

	$pairing->c8swapper

Gets/sets an iterator through the different exchanges of players in the two halves of the bracket.

=cut

sub c8swapper {
    my $self      = shift;
    my $c8swapper = shift;
    if ( defined $c8swapper ) { $self->{c8swapper} = $c8swapper; }
    elsif ( $self->{c8swapper} ) { return $self->{c8swapper}; }
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

    perldoc Games::Tournament::Swiss::Bracket

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Games-Tournament-Swiss-Bracket>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Games-Tournament-Swiss-Bracket>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Games-Tournament-Swiss-Bracket>

=item * Search CPAN

L<http://search.cpan.org/dist/Games-Tournament-Swiss-Bracket>

=back

=head1 ACKNOWLEDGEMENTS

See L<http://www.fide.com/official/handbook.asp?level=C04> for the FIDE's Swiss rules.

=head1 COPYRIGHT & LICENSE

Copyright 2006 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;    # End of Games::Tournament::Swiss::Bracket

# vim: set ts=8 sts=4 sw=4 noet:
