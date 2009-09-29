#line 1
package Mouse::Object;
use strict;
use warnings;

use Mouse::Util qw(does dump);

sub new {
    my $class = shift;

    $class->throw_error('Cannot call new() on an instance') if ref $class;

    my $args = $class->BUILDARGS(@_);

    my $instance = Mouse::Meta::Class->initialize($class)->new_object($args);
    $instance->BUILDALL($args);
    return $instance;
}

sub BUILDARGS {
    my $class = shift;

    if (scalar @_ == 1) {
        (ref($_[0]) eq 'HASH')
            || $class->meta->throw_error("Single parameters to new() must be a HASH ref");

        return {%{$_[0]}};
    }
    else {
        return {@_};
    }
}

sub DESTROY {
    my $self = shift;

    $self->DEMOLISHALL();
}

sub BUILDALL {
    my $self = shift;

    # short circuit
    return unless $self->can('BUILD');

    for my $class (reverse $self->meta->linearized_isa) {
        my $build = do{ no strict 'refs'; *{ $class . '::BUILD' }{CODE} }
            or next;

        $self->$build(@_);
    }
    return;
}

sub DEMOLISHALL {
    my $self = shift;

    # short circuit
    return unless $self->can('DEMOLISH');

    # We cannot count on being able to retrieve a previously made
    # metaclass, _or_ being able to make a new one during global
    # destruction. However, we should still be able to use mro at
    # that time (at least tests suggest so ;)

    foreach my $class (@{ Mouse::Util::get_linear_isa(ref $self) }) {
        my $demolish = do{ no strict 'refs'; *{ $class . '::DEMOLISH'}{CODE} }
            or next;

        $self->$demolish();
    }
    return;
}

1;

__END__

#line 136

