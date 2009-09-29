#line 1
package Class::Method::Modifiers::Fast;
use strict;
use warnings;
use Data::Util;
our $VERSION = '0.03';

use base 'Exporter';
our @EXPORT      = qw(before after around);
our @EXPORT_OK   = @EXPORT;
our %EXPORT_TAGS = (
    moose => [qw(before after around)],
    all   => \@EXPORT_OK,
);

use Carp 'confess';

sub _install_modifier {
    my $into     = shift;
    my $type     = shift;
    my $modifier = pop;
    my @names    = @_;

    foreach my $name (@names) {
        my $method = Data::Util::get_code_ref( $into, $name );

        if ( !$method || !Data::Util::subroutine_modifier($method) ) {

            unless ($method) {
                $method = $into->can($name)
                    or confess
                    "The method '$name' is not found in the inheritance hierarchy for class $into";
            }
            $method = Data::Util::modify_subroutine( $method,
                $type => [$modifier] );

            no warnings 'redefine';
            Data::Util::install_subroutine( $into, $name => $method );
        }
        else {
            Data::Util::subroutine_modifier( $method, $type => $modifier );
        }
    }
    return;
}

sub before {
    _install_modifier( scalar(caller), 'before', @_ );
}

sub after {
    _install_modifier( scalar(caller), 'after', @_ );
}

sub around {
    _install_modifier( scalar(caller), 'around', @_ );
}

1;

__END__

#line 163
