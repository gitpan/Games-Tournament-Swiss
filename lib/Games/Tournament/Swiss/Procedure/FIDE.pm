package Games::Tournament::Swiss::Procedure::FIDE;

# Last Edit: 2007 Apr 04, 02:58:10 PM
# $Id: $

use warnings;
use strict;

use constant ROLES      => @Games::Tournament::Swiss::Config::roles;
use constant FIRSTROUND => $Games::Tournament::Swiss::Config::firstround;

use base qw/Games::Tournament::Swiss/;
use Games::Tournament::Contestant::Swiss;

use constant {
    C1        => 'C1',
    C2        => 'C2',
    C3        => 'C3',
    C4        => 'C4',
    C5        => 'C5',
    C6PAIRS   => 'C6PAIRS',
    C6OTHERS  => 'C6OTHERS',
    C7        => 'C7',
    C8        => 'C8',
    C9        => 'C9',
    C10       => 'C10',
    REPEATC10 => 'REPEATC10',
    C11       => 'C11',
    C12       => 'C12',
    C13       => 'C13',
    C14       => 'C14',
    FLOAT     => "FLOAT",
    START     => "START",
    LAST      => "LAST",
    ERROR     => "ERROR",
    MATCH     => "MATCH",
    NEXT      => "NEXT",
    PREV      => "PREV",
    COLORS    => "COLORS",
};

=head1 NAME

Games::Tournament::Swiss::Procedure::FIDE - FIDE Swiss Rules Based on Rating 04.1

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.02';

=head1 SYNOPSIS

 $tourney = Games::Tournament::Swiss->new( rounds => 2, entrants => [ $a, $b, $c ] );
 @groups = $tourney->formBrackets;
 $pairing = $tourney->pairing( \@groups );
 @pairs = $pairing->matchPlayers( $index, $matches, %args );


    ...

=head1 DESCRIPTION

FIDE Swiss Rules C 04.1 Based on Rating describes an algorithm to pair players. The algorithm starts with the highest bracket, and then pairs each bracket in turn. ending with the lowest bracket, floating players up and down to find acceptable matches, but also undoing pairings in higher score groups, if this will help the pairing of lower score groups. This module pairs players on the basis of that algorithm.

=head1 REQUIREMENTS

I will require something

=head1 METHODS

=head2 new

 $pairing = Games::Tournament::Swiss::Procedure->new( \@groups );

Creates a FIDE C 04.1 algorithm object on a reference to a list of scoregroups ordered by score, the group with highest score first, the one with lowest score last. This object has an index accessor method to the group we are pairing at this point in the algorithm, a matches accessor to the games (cards) the algorithm has made, an incompatibles accessor to previous matches ofthe players, and XXX. This constructor is called in the Games::Tournament::Swiss::pairing method.

=cut 

sub new {
    my $self     = shift;
    my $index    = -1;
    my %args     = @_;
    my $round    = $args{round};
    my $brackets = $args{brackets};
    my $banner   = "Round $round:  ";
    for my $bracket ( 0 .. $#$brackets ) {
        my $members = $brackets->[$bracket]->members;
        my $score   = $brackets->[$bracket]->score;
        $banner .= "@{[map { $_->id } @$members]} ($score), ";
    }
    print $banner . "\n";
    return bless {
        round        => $round,
        index        => $index,
        brackets     => $brackets,
        whoPlayedWho => $args{whoPlayedWho},
        colorClashes => $args{colorClashes},
        badpair      => undef,
        byes         => $args{byes},
        matches      => []
      },
      "Games::Tournament::Swiss::Procedure";
}

=head2 matchPlayers

 @pairs = $pairing->matchPlayers;

Run the FIDE C 04.1 algorithm adding matches to $pairing->matches.

=cut 

sub matchPlayers {
    my $self    = shift;
    my %machine = (
        START, [ \&next, C1, LAST ],
        C1, [ \&c1, C2, NEXT, C13, C12 ],
        C2,      [ \&c2,      C3 ],
        C3,      [ \&c3,      C4 ],
        C4,      [ \&c4,      C5 ],
        C5,      [ \&c5,      C6PAIRS ],
        C6PAIRS, [ \&c6pairs, C6OTHERS, COLORS, C7 ],
        C6OTHERS, [ \&c6others, NEXT, C1 ],
        C7, [ \&c7, C6PAIRS, C8, C9, REPEATC10 ],
        C8,  [ \&c8,  C5, C9,  C10 ],
        C9,  [ \&c9,  C4, C10, C11 ],
        C10, [ \&c10, C7, C2,  C11 ],
        REPEATC10, [ \&repeatc10, C2, C11 ],
        C11, [ \&c11, C3, C13, C14 ],
        C12,    [ \&c12,    PREV ],
        C13,    [ \&c13,    C7, C1 ],
        C14,    [ \&c14,    C1, C4 ],
        COLORS, [ \&colors, C6OTHERS ],
        NEXT,   [ \&next,   C1, LAST ],
        PREV,   [ \&prev,   C1 ],
        LAST,  [ undef, LAST ],
        ERROR, [ undef, ERROR ],
    );
    my $state    = START;
    my $oldState = $state;
    my %args     = %$self;
    for ( ; ; ) {
        my $transitions = $machine{$state};
        my ( $action, @alterStates ) = @$transitions;
        $oldState = $state;
        ( $state, %args ) = $action->( $self, %args ) if $action;
        if ( $state eq ERROR ) {
            die
qq/Pairing error: @{[map { "$_ => $args{$_}, " } keys %args]} Pairing NOT complete\n/;
        }
        if ( $state eq LAST ) { print "Pairing complete\n"; return %args; }
        die "No transition defined from $oldState to $state"
          unless grep m/$state/, @alterStates;
    }
}


=head2 next

 $pairing->next;

Match the next bracket. End if this is the last bracket.

=cut 

sub next {
    my $self  = shift;
    my %args  = @_;
    my $index = delete $args{index};
    $index = -1 unless defined $index;
    my $groups = $args{brackets};
    return (LAST, index => $index, %args) if $index >= $#$groups;
    my $next    = $index + 1;
    my $members = $groups->[$next]->members;
    print "Next, Bracket "
      . ( $next + 1 )
      . ": @{[map { $_->{id} } @$members]}\n";
    return C1, index => $next, %args;
}


=head2 prev

 $pairing->prev;

Match the previous bracket. End if this is the first bracket.

=cut 

sub prev {
    my $self  = shift;
    my $index = $self->{index};
    $index = -1 unless defined $index;
    my $groups = $self->brackets;
    return LAST if $index <= 0;
    $self->{index} = $index - 1;
    print "prev,\tindex " . $self->{index} . "\n";
    return C1;
}


=head2 c1

 $pairing->c1;

If the score group contains a player for whom no opponent can be found (B1,B2), and if this player is a downfloater, go to C12 to find another player to downfloat instead. Or if this is the last group, go to C13. Otherwise, downfloat the unpairable player.

=cut 

sub c1 {
    my $self          = shift;
    my %args          = @_;
    my $index         = $args{index};
    my $groups        = $args{brackets};
    my $alreadyPlayed = $args{whoPlayedWho};
    my $colorClashes  = $args{colorClashes};
    my $group         = $groups->[$index];
    my $members       = $group->residents;
    print "C1,  ";
    my @unpairables;
    if ( @$members == 1 ) {
        push @unpairables, $members->[0];
    }
    else {
      PLAYER: for my $player (@$members) {
            my $failures  = 0;
            my @possibles = grep { $_ != $player } @$members;
            my @ids       = map { $_->pairingNumber } @possibles;
            foreach my $possibility ( @$alreadyPlayed{@ids} ) {
                next PLAYER
                  unless grep { $_ == $player->pairingNumber }
                  keys %$possibility;
                $failures++;
            }
            if ( $failures >= @possibles or @possibles == 0 ) {
                print "B1a: NOK. " unless @unpairables;
                print $player->id . " ";
                push @unpairables, $player;
            }
            foreach my $possibility ( @$colorClashes{@ids} ) {
                next PLAYER
                  unless grep { $_ == $player->pairingNumber }
                  keys %$possibility;
                $failures++;
            }
            if ( $failures >= @possibles or @possibles == 0 ) {
                print "B2a: NOK. " unless @unpairables;
                print $player->id . " ";
                push @unpairables, $player;
            }
        }
    }
    if (@unpairables) {
        for my $out (@unpairables) {
            return ( C12, %args )
              if $out->floating
              and $out->floating eq 'Down';
        }
        if ( $#$groups == $index ) {
            print "\n";
            return C13, %args;
        }
        else {

            # $self->float( floaters => \@unpairables, direction => "Down",
            #     	    %args );
            for my $out (@unpairables) {
                $group->exit($out);
                $out->floating('Down');
                $groups->[ $index + 1 ]->entry($out);
            }
            my ( $bracket, $next ) = ( $index + 1, $index + 2 );
            print
qq/Bracket $bracket. Floating @{[map {$_->id." ".$_->floating.", "} @unpairables]} /;
            print
"[$bracket] @{[map {$_->id} @{$groups->[$index]->members}]} => [$next] @{[map {$_->id} @{$groups->[$index+1]->members}]}\n";
            if ( @unpairables == @$members ) {

                # my $matches = delete $args{matches};
                # $matches->[$index] = undef unless $matches->[$index];
                # return NEXT, matches => $matches, %args;
                return NEXT, %args;
            }
        }
    }
    print "B1,2 test: ok, no unpairables\n" unless @unpairables;
    return C2, %args;
}


=head2 float

 $pairing->float( $index, $unpairables, $direction )

Float a single contestant, or an anonymous array of contestants, from the ${index}th bracket, but do not remove them from its membership list. If half or more of the bracket is then made up of downfloaters, the bracket is treated as homogeneous. The default $direction is 'Down.' If $direction = 'Up', they are floated up. A3

=cut 

sub float {
    my $self     = shift;
    my %args     = @_;
    my $index    = $args{index};
    my $groups   = delete $args{brackets};
    my $floaters = $args{floaters};
    print "Floating ";
    my @floaters = ref $floaters eq 'ARRAY' ? @$floaters : ($floaters);
    my $direction = $args{direction};
    print "$direction: ";
    my $oldBracket = $groups->[$index];
    my ( $newBracket, $newIndex );

    for my $floater (@floaters) {
        print "$floater->{id} ";
        if ( $direction =~ m/^Up/ ) {
            die "Player $floater->{id} => @{[$index-1]}th group"
              unless $index - 1 >= 0;
        }
        else {
            die "Player $floater->{id} => @{[$index+2]}th group"
              unless $index + 1 <= $#$groups;
        }
        $floater->floating($direction);
        $oldBracket->emigrants($floater);
        $newIndex = $direction =~ m/^Up/i ? $index - 1 : $index + 1;
        if ( $newIndex <= $#$groups ) { $newBracket = $groups->[$newIndex]; }
        else {
            $newBracket = Games::Tournament::Swiss::Bracket->new(
                score   => $groups->[$newIndex]->{score},
                members => undef
            );
        }
        my $members = $newBracket->members;
        unshift @$members, $floater;
        $newBracket->members($members);
        $newBracket->immigrants($floater);
        splice( @$groups, $newIndex, 1, $newBracket );
    }
    my ( $bracketN, $newBracketN ) = ( $index + 1, $newIndex + 1 );

# print "[$bracketN] @{[map {$_->id} @{$groups->[$index]->members}]} => [$newBracketN] @{[map {$_->id} @{$groups->[$newIndex]->members}]}\n";
    print
"[$bracketN] @{[map {$_->id} @{$oldBracket->members}]} => [$newBracketN] @{[map {$_->id} @{$newBracket->members}]}\n";
    return $groups;
}


=head2 reassign

 $pairing->reassign( $unpairables, $direction )

Float a single contestant, or an anonymous array of contestants, to the next bracket and remove them from the old bracket's membership list, if they were a member of the list. If half or more of the bracket is then made up of downfloaters, the bracket is treated as homogeneous. The default $direction is 'Down.' If $direction = 'Up', they are floated up. A3

=cut 

sub reassign {
    my $self     = shift;
    my $index    = $self->{index};
    my $floaters = shift;
    print "Reassigning ";
    my $direction = shift;
    print "$direction: ";
    $self->float( $floaters, $direction );
    my $groups     = $self->brackets;
    my $oldBracket = $groups->[$index];
    my $oldMembers = $oldBracket->members;
    my @floaters   = ref $floaters eq 'ARRAY' ? @$floaters : ($floaters);
    print "[$index] @{[map {$_->id} @{$groups->[$index]->members}]} => ";

    for my $floater (@floaters) {
        my @positions =
          grep { $oldMembers->[$_]->{id} == $floater->{id} } 0 .. $#$oldMembers;
        if (@positions) {
            die
              "floater $floater->{id} was in ${index}th bracket more than once"
              if $#positions;
            my $position;
            $position = shift @positions;
            splice( @$oldMembers, $position, 1 );
            $oldBracket->members($oldMembers);
            splice( @$groups, $index, 1, $oldBracket );
        }
    }
    $self->brackets($groups);
    print "[$index] @{[map {$_->id} @{$groups->[$index]->members}]}\n";
    return;
}


=head2 c2

 $pairing->c2

Determine x according to A8.

=cut 

sub c2 {
    my $self   = shift;
    my %args   = @_;
    my $index  = $args{index};
    my $groups = $args{brackets};
    my $group  = $groups->[$index];
    my $x      = $group->x;
    $group->xprime( $group->x );
    print "C2, x=$x\n";
    return C3, %args;
}


=head2 c3

 $pairing->c3

Determine p according to A6.

=cut 

sub c3 {
    my $self   = shift;
    my %args   = @_;
    my $index  = $args{index};
    my $groups = $args{brackets};
    my $group  = $groups->[$index];
    my $p      = $group->p;
    $group->pprime( $group->p );
    print "C3, p=$p\t";
    if ( $group->hetero ) { print "Heterogeneous.\n"; }
    else { print "Homogeneous.\n"; }
    return C4, %args;
}


=head2 c4

 $pairing->c4

The highest players in S1, the others in S2.

=cut 

sub c4 {
    my $self    = shift;
    my %args    = @_;
    my $index   = $args{index};
    my $groups  = $args{brackets};
    my $group   = $groups->[$index];
    my $members = $group->members;
    my $s1      = $group->resetS1;
    my $s2      = $group->resetS2;
    print
      "C4, S1 & S2: @{[map{ $_->{id} } @$s1]} & @{[map{ $_->{id} } @$s2]}\n";

    for my $member ( @{ $group->s2 } ) {
        die "$member->{id} was in ${index}th bracket more than once"
          if ( grep { $_->id == $member->id } @{ $group->s2 } ) > 1;
    }
    return C5, %args;
}


=head2 c5

 $pairing->c5

Order the players in S1 and S2 according to A2.

=cut 

sub c5 {
    my $self   = shift;
    my %args   = @_;
    my $index  = $args{index};
    my $groups = $args{brackets};
    # die "index is $index, but only @{[1+$#$groups]} groups"
    return ERROR if $index >= @$groups;
    my $group   = $groups->[$index];
    my $members = $group->members;
    my $x       = $group->xprime;
    my $s1      = $group->s1;
    my $s2      = $group->s2;
    my @s1      = $self->rank(@$s1);
    my @s2      = $self->rank(@$s2);
    print "C5, ordered: @{[map{ $_->{id} } @s1]} & @{[map{ $_->{id} } @s2]}\n";
    # $group->members([@s1, @s2]);
    $group->s1( \@s1 );
    $group->s2( \@s2 );
    for my $member ( @{ $group->s2 } ) {
        die "$member->{id} was in ${index}th bracket more than once"
          if ( grep { $_->id == $member->id } @{ $group->s2 } ) > 1;
    }
    splice @$groups, $index, 1, $group;
    $self->brackets($groups);
    return C6PAIRS, %args;
}


=head2 c6pairs

 Games::Tournament::Swiss::Procedure->c6pairs($group, $matches)

Pair the p players in the top half of the scoregroup in order with their counterparts in the bottom half, and return an array of tentative Games::Tournament::Card matches if B1, B2 and, under some circumstances, B5 and B6 tests pass, and in addition, none of the UNpaired players in a homogeneous bracket were downfloated in the round before (B5) or the round before that (B6), or there is only one UNpaired, previously-downfloated player in a heterogeneous group, following Bill Gletsos' advice at http://chesschat.org/showpost.php?p=142260&postcount=158.

=cut 

sub c6pairs {
    my $self   = shift;
    my %args   = @_;
    my $index  = $args{index};
    my $groups = $args{brackets};
    my $group  = $groups->[$index];
    my $p      = $group->pprime;
    my $s1     = $group->s1;
    my $s2     = $group->s2;
    print "C6, ";
    die "@{[$#$s1+1]} players in group $index\'s S1, only @{[$#$s2+1]} in S2."
      if $#$s1 > $#$s2;
    my $whoPlayedWho = $args{whoPlayedWho};
    my $colorClashes = $args{colorClashes};
    delete $args{badpair};
    my $test = 0;
    for my $pos (0..$p-1)
    {
	$test += defined $whoPlayedWho->{$s1->[$pos]->id}->{$s2->[$pos]->id};
	if ( $test ) { print "B1a: table " . ($pos+1) . " NOK\n";
	return C7, badpair => $pos, %args }
    }
    for my $pos (0..$p-1)
    {
	$test += defined $colorClashes->{$s1->[$pos]->id}->{$s2->[$pos]->id};
	if ( $test ) { print "B2a: table " . ($pos+1) . " NOK\n";
	return C7, badpair => $pos, %args }
    }
    for my $pos (0..$p-1)
    {
	$test += ( $s1->[$pos]->preference->strength eq 'Absolute' and
		$s2->[$pos]->preference->strength eq 'Absolute' and
		$s1->[$pos]->preference->role eq $s2->[$pos]->preference->role);
	if ( $test ) { print "B2: table " . ($pos+1) . " NOK\n";
	return C7, badpair => $pos, %args }
    }
    my $x = $group->xprime;
    for my $pos ( 0 .. $p - 1 ) {
        $test +=
          (       defined $s1->[$pos]->preference->role
              and defined $s2->[$pos]->preference->role
              and $s1->[$pos]->preference->role eq
              $s2->[$pos]->preference->role );
        if ( $test > $x ) {
            print "B4: x=$x, table " . ( $pos + 1 ) . " NOK\n";
            return C7, badpair => $pos, %args;
        }
    }
    for my $refloat (qw/B5 B6/) {
        my %round = ( B5 => 1, B6 => 2 );
        if ( $group->{criterion}->{$refloat} eq 'Up&Down' ) {
            for my $pos ( 0 .. $p - 1 ) {
                my @pair = map { $_->[$pos] } ( $s1, $s2 );
                my @floats = map { $_->{floats}->[ -$round{$refloat} ] } @pair;
                my @violation = ( 'Down', 'Up' );
                my ( @mess, @violators );
                if (    $pair[0]->score > $pair[1]->score
                    and @violators = grep { $floats[$_] eq $violation[$_] } 0,
                    1 )
                {
                    my @mess =
                      map { $pair[$_]->id . " floated " . $floats[$_] . "," }
                      @violators;
                    print "$refloat: NOK. @mess $round{$refloat} rounds ago\n";
                    return C7, badpair => $pos, %args;
                }
            }
            for my $pos ( $p .. $#$s2 ) {
                my $floated = $s2->[$pos]->{floats}->[ -$round{$refloat} ];
                next if not $floated or $floated =~ m/^No/;

                # my $floating = $s1->[$pos]->floating;
                if ( $floated eq 'Down'
                    and ( not $group->hetero or $p == $#$s2 ) )
                {
                    print
"$refloat: NOK. @{[$s2->[$pos]->id]} floated $floated $round{$refloat} rounds ago\n";
                    return C7, badpair => $pos, %args;
                }
            }
        }
        elsif ( $group->{criterion}->{$refloat} eq 'Up' ) {
            for my $pos ( 0 .. $p - 1 ) {
                my @pair   = map { $_->[$pos] } ( $s1, $s2 );
                my @floats = map { $_->{floats}->[-$round{$refloat}] || 'None'}
			    @pair;
		die "checking for undefined float" if grep { not defined $_ } @floats;
                if ( $pair[0]->score > $pair[1]->score and $floats[1] eq 'Up' )
                {
                    print "$refloat: NOK. "
                      . $pair[1]->id
                      . " floated Up $round{$refloat} rounds ago\n";
                    return C7, badpair => $pos, %args;
                }
            }
        }
    }
    for my $pos ( 0 .. $p - 1 ) {
        my @floater = grep { $_->floating } $s1->[$pos], $s2->[$pos];
        next if not @floater or @floater == 2;
        my $direction = $floater[0]->floating;
        my @other = grep { $_ != $floater[0] } $s1->[$pos], $s2->[$pos];
        my $otherdirection = $direction eq 'Up' ? 'Down' : 'Up';
        $other[0]->floating($otherdirection) if @other;
    }
    print "$p tables paired. ";
    return ( COLORS, %args ) if $p;
    return C6OTHERS, %args;
}


=head2 c6others

 Games::Tournament::Swiss::Procedure->c6others($group, $matches)

After pairing players, if there are remaining players in a homogeneous group, float them down to the next score group and continue with C1. In a heterogeneous group, start at C2 with the remaining players, now a homogeneous remainder group. XXX I am restarting at C1, rather than C2, to handle byes. See the problem http://chesschat.org/showpost.php?p=140199&postcount=150, which may be connected, even though C1 leads to C12 only if the player was moved down! TODO What is happening to remaininig players ins a last omogeneous group?

=cut 

sub c6others {
    my $self   = shift;
    my %args   = @_;
    my $index  = $args{index};
    my $groups = delete $args{brackets};
    print "C6others:\t";
    my $group     = $groups->[$index];
    my $p         = $group->pprime;
    my $s2        = $group->s2;
    my @nonpaired = @$s2[ $p .. $#$s2 ];
    if ( @nonpaired and $group->hetero == 0 and $index < $#$groups ) {
        delete $args{direction};
        for my $out (@nonpaired) {
            $groups->[$index]->exit($out);
            $out->floating('Down');
            $groups->[ $index + 1 ]->entry($out);
        }
        my ( $bracket, $next ) = ( $index + 1, $index + 2 );
        print
"Floating remaining @{[map {$_->id} @nonpaired]} Down. [$bracket] @{[map {$_->id} @{$group->members}]} => [$next] @{[map {$_->id} @{$groups->[$index+1]->members}]}\n";

# $self->float( floaters => \@nonpaired, direction => "Down", index => $index, brackets => $groups, %args );
        return NEXT, index => $index + 1, brackets => $groups, %args;
    }
    elsif (@nonpaired) {
        print
"Remainder Group, Bracket @{[$index+1]}: @{[map{$_->id}@nonpaired]}\n";
        my $remainderGroup = Games::Tournament::Swiss::Bracket->new(
            score       => $group->score,
            members     => \@nonpaired,
            remainderof => $group
        );
        splice( @$groups, $index, 1, $remainderGroup );
        return C1, index => $index, brackets => $groups, %args;
    }
    print "no non-paired players\n";
    return NEXT, brackets => $groups, %args;
}


=head2 c7

	$next = $pairing->c7
	while ( my @s2 = &$next )
	{
	    create match cards unless this permutation is incompatible;
	}

Apply a new transposition of S2 according to D1 and restart at C6.

=cut 

sub c7 {
    my $self   = shift;
    my %args   = @_;
    my $index  = $args{index};
    my $groups = delete $args{brackets};
    print "C7, ";
    die "index is $index, but only @{[1+$#$groups]} groups"
      if $index >= @$groups;
    my $group   = $groups->[$index];
    my $s1      = $group->s1;
    my $s2      = $group->s2;
    my $badpair = $args{badpair};
    my @newS2   = $group->c7shuffler($badpair);

    unless (@newS2) {
        print "last transposition\n";
        $group->resetS1;
        $group->resetS2;
        return ( C8, brackets => $groups, %args ) unless $group->hetero;
        return ( REPEATC10, brackets => $groups, %args )
          if $group->hetero
          and $group->{didC10};
        return C9, brackets => $groups, %args;
    }
    print "         @{[map { $_->id } @newS2]}\n";
    $group->s2( \@newS2 );
    $group->members( [ @$s1, @newS2 ] );
    splice @$groups, $index, 1, $group;
    # print "\n";
    return C6PAIRS, brackets => $groups, %args;
}


=head2 c8

	$next = $pairing->c8
	while ( my ($s1, $s2) = &$next )
	{
	    create match cards unless this exchange is incompatible;
	}

In case of a homogeneous (remainder) group: apply a new exchange between S1 and S2 according to D2. Restart at C5.

=cut 

sub c8 {
    my $self   = shift;
    my %args   = @_;
    my $index  = $args{index};
    my $groups = delete $args{brackets};
    print "C8, ";
    die "index is $index, but only @{[1+$#$groups]} groups"
      if $index >= @$groups;
    my $group = $groups->[$index];
    my $swapper;

    if ( $group->c8swapper ) {
        $swapper = $group->c8swapper;
    }
    else {
        $swapper = $group->c8iterator;
        $group->c8swapper($swapper);
    }
    my @newMembers = &$swapper;
    # die "Group $index ran out of S1,S2 exchanges" unless @newMembers;
    unless (@newMembers) {
        print "last S1,S2 exchange\n";
        return ( C10, brackets => $groups, %args )
          if $group->{remainderof}
          and not $group->{didC10};
        return ( C9, brackets => $groups, %args ) unless $group->{hetero};
        return ( ERROR, "Only homogeneous (remainder) groups here" );
    }
    # $group->{members} = \@newMembers;
    my $p  = $group->p;
    my @s1 = @newMembers[ 0 .. $p - 1 ];
    my @s2 = @newMembers[ $p .. $#newMembers ];
    $group->s1( \@s1 );
    $group->s2( \@s2 );
    print "@{[map { $_->id } @s1]}, @{[map { $_->id } @s2]}\t";
    splice @$groups, $index, 1, $group;
    $self->{brackets} = $groups;
    print "\n";
    return C5, brackets => $groups, %args;
}


=head2 c9

 Games::Tournament::Swiss::Procedure->c9

Drop, in order, criterion B6 (no identical float to 2 rounds before) and B5 (no identical float to previous round) for downfloats and restart at C4.

=cut 

sub c9 {
    my $self    = shift;
    my %args    = @_;
    my $index   = $args{index};
    my $groups  = delete $args{brackets};
    my $group   = $groups->[$index];
    my $bracket = $index + 1;
    print "C9, ";
    if ( $group->{criterion}->{B6} eq 'Up&Down' ) {
        $group->{criterion}->{B6} = 'Up';
        print "Dropping B6 for Downfloats\n";
        return C4, brackets => $groups, %args;
    }
    elsif ( $group->{criterion}->{B5} eq 'Up&Down' ) {
        $group->{criterion}->{B5} = 'Up';
        print "Dropping B5 for Downfloats\n";
        return C4, brackets => $groups, %args;
    }
    # my $index = $self->{index};
    # my $group = $self->brackets->[$index];
    # if ( $group->{remainderof} ) { return C10; }
    # else { return C11; }
    print " B5,6 already dropped for Downfloats in Bracket $bracket.\n";
    return C11, brackets => $groups, %args;
}


=head2 c10

 Games::Tournament::Swiss::Procedure->c10

In case of a homogeneous remainder group: undo the pairing of the lowest moved down player paired and try to find a different opponent for this player by restarting at C7. If no alternative pairing for this player exists then drop criterion B6 first and then B5 for upfloats and restart at C2. We do this in a separate subroutine, repeatc10.

=cut 

sub c10 {
    my $self   = shift;
    my %args   = @_;
    my $index  = $args{index};
    my $groups = delete $args{brackets};
    my $group  = $groups->[$index];
    $group = $group->{remainderof};
    die "No group $index to undo"
      unless $group
      and $group->isa('Games::Tournament::Swiss::Bracket');
    my $s1 = $group->s1;
    my $s2 = $group->s2;
    print "C10,re-pairing Player $s1->[-1]->{id} in Bracket @{[$index+1]}\n";
    for my $member (@$s2) { $member->floating(''); }
    my $matches = delete $args{matches};
    pop @$matches;
    $group->{didC10} = 1;
    return C7, matches => $matches, brackets => $groups, %args;
}


=head2 repeatc10

 Games::Tournament::Swiss::Procedure->c10repeat

In C10, the pairing of the lowest moved down player of a heterogeneous group was undone and a different opponent was sought for this player by restarting at C7. Assuming no alternative pairing for this player was found, in REPEATC10, drop criterion B6 first and then B5 for upfloats and restart at C2. 

=cut 

sub repeatc10 {
    my $self    = shift;
    my %args    = @_;
    my $index   = $args{index};
    my $groups  = $args{brackets};
    my $bracket = $groups->[$index];
    print "C10,again\t";
    if ( $bracket->{didC10} ) {
        if ( $bracket->{criterion}->{B6} ) {
            $bracket->{criterion}->{B6} = 0;
            print "Dropping B6 for Upfloats\n";
            return C2, %args;
        }
        elsif ( $bracket->{criterion}->{B5} ) {
            $bracket->{criterion}->{B5} = 0;
            print "Dropping B5 for Upfloats\n";
            return C2, %args;
        }
    }
    # $self->brackets->[$index]->{didC10} = 0;
    return C11, %args;
}


=head2 c11

 Games::Tournament::Swiss::Procedure->c11

As long as x (xprime) is less than p: increase it by 1. When pairing a remainder group undo all pairings of players moved down also. Restart at C3.
        
=cut 

sub c11 {
    my $self    = shift;
    my %args    = @_;
    my $index   = $args{index};
    my $groups  = delete $args{brackets};
    my $group   = $groups->[$index];
    my $bracket = $index + 1;
    my $p       = $group->p;
    my $x       = $group->x;
    my $xprime  = $group->xprime;
    $xprime = defined $xprime ? $xprime + 1 : $x;
    $group->xprime($xprime);

    unless ( $xprime > $p ) {
        print "C11, x=$xprime, ";
        my $matches = delete $args{matches};
        if (    $group->{remainderof}
            and $group->{remainderof}->isa("Games::Tournament::Swiss::Bracket")
          )
        {
            $group = $group->{remainderof};
            delete $matches->[$index];
            print "Undoing all Bracket $bracket matches\n";
        }
        $group->{c8swapper} = $group->c8iterator;
        return C3, matches => $matches, brackets => $groups, %args;
    }
    print "C11, x=p=$p already, no more x increases in Bracket $bracket.\n";
    return ( C14, brackets => $groups, %args ) if $index < $#$groups;
    return C13, brackets => $groups, %args;
}


=head2 c12

 Games::Tournament::Swiss::Procedure->c12

If the group contains a player who cannot be paired without violating B1 or B2 and this is a heterogeneous group, undo the pairing of the previous score bracket. If in this previous score bracket a pairing can be made whereby another player will be moved down to the current one, and this now allows p pairing to be made then this pairing in the previous score bracket will be accepted.

=cut 

sub c12 {
    my $self   = shift;
    my %args   = @_;
    my $index  = $args{index};
    my $groups = delete $args{brackets};
    print "C12,\n";
    # die "Undoing pairing in group[index-1] but index=0; " if $index == 0;
    my $group = $groups->[$index];
    print "Heterogenous group $index now homogeneous\n" unless $group->hetero;
    my $previous = $index - 1;
    die "No $previous group to undo"
      unless @{ $groups->[ $index - 1 ]->members };
    my $oldMatches  = pop @{ $self->matches };
    my $s1          = $groups->[$previous]->s1;
    my $newFloaters = $groups->[$previous]->s2;
    $groups->[$previous]->members($s1);
    my @oldFloaters = $group->downFloaters;
    $self->unfloat( \@oldFloaters );
    $self->urFloat( $previous, $newFloaters, "Down" );
    return PREV, brackets => $groups, %args;
}


=head2 backtrack

 Games::Tournament::Swiss::Procedure->backtrack

If the group contains a player who cannot be paired without violating B1 or B2, the pairing of the previous score bracket may be undone, in some circumstances. Another pairing may then be sought in that previous score bracket which will allow a pairing in this score bracket. This backtracking may or may not involve unfloating downfloated players.)

=cut 

sub backtrack {
    my $self  = shift;
    my $index = $self->{index};
    print "backtrack:\t";
    die "Undoing pairing in group[index-1] but index=0; " if $index == 0;
    my $groups   = $self->brackets;
    my $bracket  = $groups->[$index];
    my $previous = $index - 1;
    warn "No $previous group to undo"
      unless @{ $groups->[ $index - 1 ]->members };
    my $oldPairing = pop @{ $self->matches };
    $self->{index} = $previous;
    $self->c7;
    return ERROR;
}


=head2 c13

 Games::Tournament::Swiss::Procedure->c13

If the lowest score group contains a player who cannot be paired without violating B1 or B2 or who, if they are the only player in the group, cannot be given a bye (B2b), the pairing of the penultimate score bracket is undone.  Try to find another pairing in the penultimate score bracket which will allow a pairing in the lowest score bracket. If in the penultimate score bracket p becomes zero (i.e. no pairing can be found which will allow a correct pairing for the lowest score bracket) then the two lowest score brackets are joined into a new lowest score bracket. Because now another score bracket is the penultimate one C13 can be repeated until an acceptable pairing is obtained. TODO The first condition subsumes B2b. I need to move the bye granting code out of here. XXX I am popping the last match off if $index == $#$matches. Perhaps all the players from the penultimate bracket were floated down. Do I need to identify which bracket the  matches more securely? Perhaps for more than one bracket, all were floated down.

=cut 

sub c13 {
    my $self    = shift;
    my %args    = @_;
    my $index   = delete $args{index};
    my $groups  = delete $args{brackets};
    my $matches = delete $args{matches};
    print "C13,\t";
    die "$index is not index of last bracket $#$groups"
      unless $index == $#$groups;
    my $group   = $groups->[$index];
    my $members = $group->members;

    if ( $#$members == 0 ) {
        my $byer = $members->[0]->id;
        print "only player $byer ";
        my $byes = $args{byes};
        if ( my @id = grep { $_ == $byer } keys %$byes ) {
            my $id = $id[0];
            print "but, player $id had bye in round $byes->{$id}\n";
        }
        else {
            my $byes  = delete $args{byes};
            my $round = $args{round};
            my $byer  = $members->[0];
            my $id    = $byer->id;
            push @{ $matches->[$index] },
              Games::Tournament::Card->new(
                round       => $round,
                result      => { Bye => 'Bye' },
                contestants => { Bye => $byer }
              );
            print "bye:\t$id\n";
            $byes->{$id} = $round;
            return LAST,
              index    => $index,
              brackets => $groups,
              matches  => $matches,
              byes     => $byes,
              %args;
        }
    }
    my $penultimate = $groups->[ $index - 1 ];
    my ( $last, $penult ) = ( $index + 1, $index );
    my $pprime = $penultimate->pprime;
    #die "index=$index,but matches=$#$matches"
    #    		unless $#$matches == $index;
    delete $matches->[ $index - 1 ];
    print "Undoing Bracket $index matches. ";
    my @lastMemberIds = map { $_->id } @$members;
    my $residents     = $group->residents;
    my @floaters      = $group->downFloaters;
    my @others        = grep {
        my $resident = $_;
        not grep { $resident->id == $_->id } @floaters
    } @$residents;
    $groups->[$index]->exit($_) for @floaters;
    $_->floating('')            for @floaters;
    $penultimate->reentry($_)   for @floaters;
    delete $args{badpair};
    if ( $pprime > 0 ) {
        print "Re-pairing Bracket $penult, p=$pprime. ";
        print
qq/Unfloating @{[map {$_->id." ".$_->floating.", "} @floaters]} back. /
          if @floaters;
        print
"Bracket $last: @{[map {$_->id} @{$groups->[$index]->members}]} => Bracket $penult: @{[map {$_->id} @{$groups->[$index-1]->members}]}"
          if @floaters;
        print "\n";
        return C7,
          index   => @{ [ $index - 1 ] },
          badpair => @{ [ $pprime - 1 ] },
          matches  => $matches,
          brackets => $groups,
          %args;
    }
    else {
        $groups->[$index]->exit($_)  for @others;
        $_->floating('Up')           for @others;
        $penultimate->entry($_)      for @others;
        $penultimate->naturalize($_) for @others;
        print "Joining Bracket $penult, $last. ";
        print
"Bracket $last: @lastMemberIds => Bracket $penult: @{[map {$_->id} @{$groups->[$index-1]->members}]}\n";
        splice @$groups, $index, 1;
        return C1,
          index => @{ [ $index - 1 ] },
          matches  => $matches,
          brackets => $groups,
          %args;
    }
}


=head2 unfloat

 $self->unfloat(\@downFloaters)

Floaters may be substituted (C12,C13), requiring the pairing of the previous bracket to be undone. 'unfloat' returns a floater (or an anonymous array of them) to the bracket it came from, TODO if that bracket will accept it. That will depend on whether an opponent for it can be found in that bracket.

=cut 

sub unfloat {
    my $self     = shift;
    my $floaters = shift;
    my @floaters = ref $floaters eq 'ARRAY' ? @$floaters : ($floaters);
    print qq/Unfloating @{[map {$_->floating." ".$_->id.", "} @floaters]} /;
    my $groups     = $self->brackets;
    my $index      = $self->{index};
    my $bracketNow = $groups->[$index];
    my $membersNow = $bracketNow->members;
    my ( $prevIndex, $prevBracket );

    for my $floater (@floaters) {
        my $oldDirection = $floater->floating;

#if ($oldDirection =~ m/^Up/)
#{
#    die "Trying to return floater $floater->{id} to nonexistent @{[$index-1]}th bracket" unless $index+1 <= $#$groups;
#}
#else {
#    die "Trying to return floater $floater->{id} to nonexistent @{[$index+2]}th bracket in round $self->{round}" unless $index-1 >= 0;
#}
        $floater->floating('');
        $prevIndex = $oldDirection =~ m/^Up/i ? $index + 1 : $index - 1;
        $prevBracket = $groups->[$prevIndex];
        my @positions =
          grep { $membersNow->[$_] == $floater } 0 .. $#$membersNow;
        my $position = shift @positions;
        die "floater $floater->{id} was in ${index}th bracket more than once"
          if @positions;
        ( my $retrodirection = $floater->floating ) =~
          s/^$oldDirection,(.*)$/$1/;
        splice( @$membersNow, $position, 1 );
        $bracketNow->members($membersNow);
        splice( @$groups, $index, 1, $bracketNow );
        unshift @{ $prevBracket->members }, $floater;
    }
    splice( @$groups, $prevIndex, 1, $prevBracket );
    print
"[$index] @{[map {$_->id} @{$groups->[$index]->members}]} [$prevIndex] @{[map {$_->id} @{$groups->[$prevIndex]->members}]}\n";
    return;
}


=head2 c14

 Games::Tournament::Swiss::Procedure->c14

Decrease p (pprime) by 1 (and if the original value of x was greater than zero decrease x by 1 as well). As long as p is unequal to zero restart at C4. If p equals zero the entire score bracket is moved down to the next one. Restart with this score bracket at C1.

=cut 

sub c14 {
    my $self   = shift;
    my %args   = @_;
    my $index  = $args{index};
    my $groups = $args{brackets};
    print "C14,\t";
    my $group   = $groups->[$index];
    my $members = $group->members;
    my $p       = $group->p;
    my $x       = $group->xprime;
    my $pprime  = $group->pprime;
    if ($pprime) {
        $pprime -= 1;
        $x -= 1 if $x;
    }
    $group->pprime($pprime);
    $group->xprime($x);
    $group->{criterion}->{$_} = 'Up&Down' for qw/B5 B6/;
    print "Bracket " . ( $args{index} + 1 ) . ", now p=$pprime\n";
    return ( C4, %args ) if $pprime > 0;
    $self->float(
        floaters  => $members,
        direction => "Down",
        index     => $index,
        brackets  => $groups,
        %args
    );
    return C1, %args;
}


=head2 colors

	$next = $pairing->c7
	while ( my @s2 = &$next )
	{
	    create match cards unless this permutation is incompatible;
	}

After an acceptable pairing is achieved that doesn't violate the one-time match only principle (B1) and the 2-game maximum on difference between play in one role over that in the other role (B2), roles are allocated so as to grant the preferences of both players, or grant the stronger preference, or grant the opposite roles to those they had when they last played a round in different roles, or grant the preference of the higher ranked player, in that order. (E) A Games::Tournament::Card object, records round, contestants, (undefined) result, and floats (A4).
 

=cut 

sub colors {
    my $self       = shift;
    my %args       = @_;
    my $index      = $args{index};
    my $groups     = $args{brackets};
    my $round      = $args{round};
    my $allMatches = delete $args{matches};
    # print "Roles:\t";
    die "index is $index, but only @{[1+$#$groups]} groups"
      if $index >= @$groups;
    my $group = $groups->[$index];
    my $s1    = $group->s1;
    my $s2    = $group->s2;
    my $p     = $group->pprime;
    my @bracketMatches;
    for my $i ( 0 .. $p - 1 ) {
        my @pair = ( $s1->[$i], $s2->[$i] );
        my @rolehistory = ( map { $pair[$_]->roles } 0, 1 );
        my @round = map {
            my $r = $_;
            [ map { $_->[$r] } @rolehistory ]
        } 0 .. ($round) - ( FIRSTROUND + 1 );
        my @roundh;
        for ( 0 .. $#round ) {
            push @roundh, $round[$_]
              if 2 == grep { $_ eq (ROLES)[0] or $_ eq (ROLES)[1] }
              @{ $round[$_] };
        }
        my ( $contestants, $stronger, $diff );
        my @roles     = map { $_->preference->role } @pair;
        my @strengths = map { $_->preference->strength } @pair;
        my $rule;
        if ( not $roles[0] and not $roles[1] ) {
            ( $roles[0], $roles[1] ) = $self->randomRole;
            $rule = 'No prefs';
        }
        if ( not $roles[0] ) {
            $roles[0] =
                ( $roles[1] eq (ROLES)[1] )
              ? (ROLES)[0]
              : (ROLES)[1];
            $rule = 'No S1 pref';
        }
        if ( not $roles[1] ) {
            $roles[1] =
                ( $roles[0] eq (ROLES)[1] )
              ? (ROLES)[0]
              : (ROLES)[1];
            $rule = 'No S2 pref';
        }
        if ( $roles[0] ne $roles[1] ) {
            $contestants = { $roles[0] => $pair[0], $roles[1] => $pair[1] };
            $rule = 'E1';
        }
        elsif ( $strengths[0] ne $strengths[1] ) {
            if (
                defined(
                    $stronger = (
                        grep { $pair[$_]->preference->strength eq 'Absolute' }
                          0 .. 1
                      )[0]
                )
              )
            {
                1;
            }
            elsif (
                defined(
                    $stronger = (
                        grep { $pair[$_]->preference->strength eq 'Strong' }
                          0 .. 1
                      )[0]
                )
              )
            {
                1;
            }
            elsif (
                defined(
                    $stronger = (
                        grep { $pair[$_]->preference->strength eq 'Mild' }
                          0 .. 1
                      )[0]
                )
              )
            {
                1;
            }
            my $strongerRole = $pair[$stronger]->preference->role;
            my $weaker       = $stronger == 0 ? 1 : 0;
            my $weakerRole   = ( grep { $_ ne $strongerRole } ROLES )[0];
            $contestants = {
                $strongerRole => $pair[$stronger],
                $weakerRole   => $pair[$weaker]
            };
            $rule = 'E2';
        }
        elsif ( defined( $diff = ( grep { $_->[0] ne $_->[1] } @roundh )[0] ) )
        {
            $contestants = { $diff->[1] => $pair[0], $diff->[0] => $pair[1] };
            $rule = 'E3';
        }
        else {
            my $rankerRole = $pair[0]->preference->role;
            my $otherRole = ( grep { $_ ne $rankerRole } ROLES )[0];
            $contestants = { $rankerRole => $pair[0], $otherRole => $pair[1] };
            $rule = 'E4';
        }
        print $rule . ' '
          . $contestants->{ (ROLES)[0] }->id . "&"
          . $contestants->{ (ROLES)[1] }->id . "  ";
        my %floats =
          map { (ROLES)[$_] => $contestants->{ (ROLES)[$_] }->floating } 0 .. 1;
        push @bracketMatches,
          Games::Tournament::Card->new(
            round       => $self->round,
            result      => undef,
            contestants => $contestants,
            floats      => \%floats
          );
    }
    print "\n";
    push @{ $allMatches->[$index] }, @bracketMatches;
    return C6OTHERS, matches => $allMatches, %args;
}


=head2 brackets

	$pairing->brackets

Gets/sets all the brackets which we are pairing, as an anonymous array of score group (bracket) objects. The order of this array is important. The brackets are paired in order.

=cut

sub brackets {
    my $self     = shift;
    my $brackets = shift;
    if ( defined $brackets ) { $self->{brackets} = $brackets; }
    elsif ( $self->{brackets} ) { return $self->{brackets}; }
}


=head2 round

	$pairing->round

What round is this round's results we're pairing on the basis of?

=cut

sub round {
    my $self  = shift;
    my $round = shift;
    if ( defined $round ) { $self->{round} = $round; }
    elsif ( $self->{round} ) { return $self->{round}; }
}


=head2 matches

	$group->matches

Gets/sets the matches which we have made. Returned is an anonymous array of the matches in the round, the first element of which is an anonymous array of the matches in the first bracket, and so on for the other brackets.

=cut

sub matches {
    my $self    = shift;
    my $matches = shift;
    if ( defined $matches ) { $self->{matches} = $matches; }
    elsif ( $self->{matches} ) { return $self->{matches}; }
}


=head2 whoPlayedWho

	$group->whoPlayedWho

Gets/sets a anonymous hash, keyed on the pairing numbers of the opponents, of the preference of individual pairs of @grandmasters, if they both have the same absolute preference, and so can't play each other. This has probably been calculated by Games::Tournament::Swiss::whoPlayedWho B1a

=cut

sub whoPlayedWho {
    my $self         = shift;
    my $whoPlayedWho = shift;
    if ( defined $whoPlayedWho ) { $self->{whoPlayedWho} = $whoPlayedWho; }
    elsif ( $self->{whoPlayedWho} ) { return $self->{whoPlayedWho}; }
}


=head2 colorClashes

	$group->colorClashes

Gets/sets a anonymous hash, keyed on the pairing numbers of the opponents, of their preference, if they both have an Absolute preference for the same role and so can't play each other. This has probably been calculated by Games::Tournament::Swiss::colorClashes B2a

=cut

sub colorClashes {
    my $self         = shift;
    my $colorClashes = shift;
    if ( defined $colorClashes ) { $self->{colorClashes} = $colorClashes; }
    elsif ( $self->{colorClashes} ) { return $self->{colorClashes}; }
}


=head2 incompatibles

	$group->incompatibles

Gets/sets a anonymous hash, keyed on the pairing numbers of the opponents, of a previous round in which individual pairs of @grandmasters, if any, met. Or of their preference if they both have an Absolute preference for the same role and can't play each other. This has probably been calculated by Games::Tournament::Swiss::incompatibles. B1

=cut

sub incompatibles {
    my $self          = shift;
    my $incompatibles = shift;
    if ( defined $incompatibles ) { $self->{incompatibles} = $incompatibles; }
    elsif ( $self->{incompatibles} ) { return $self->{incompatibles}; }
}


=head2 byes

	$group->byes

Gets/sets a anonymous hash, keyed on pairing numbers of players, of a previous round in which these players had a bye. This has probably been calculated by Games::Tournament::Swiss::byes. B1

=cut

sub byes {
    my $self = shift;
    my $byes = shift;
    if ( defined $byes ) { $self->{byes} = $byes; }
    elsif ( $self->{byes} ) { return $self->{byes}; }
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

1;    # End of Games::Tournament::Swiss::Procedure

# vim: set ts=8 sts=4 sw=4 noet:
