#line 1
package Mouse;
use 5.006_002;

use strict;
use warnings;

our $VERSION = '0.37';

use Exporter;

use Carp 'confess';
use Scalar::Util 'blessed';

use Mouse::Util qw(load_class is_class_loaded get_code_package not_supported);

use Mouse::Meta::Module;
use Mouse::Meta::Class;
use Mouse::Meta::Role;
use Mouse::Meta::Attribute;
use Mouse::Object;
use Mouse::Util::TypeConstraints ();

our @ISA = qw(Exporter);

our @EXPORT = qw(
    extends with
    has
    before after around
    override super
    augment  inner

    blessed confess
);

our %is_removable = map{ $_ => undef } @EXPORT;
delete $is_removable{blessed};
delete $is_removable{confess};

sub extends { Mouse::Meta::Class->initialize(scalar caller)->superclasses(@_) }

sub has {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);
    my $name = shift;

    $meta->add_attribute($_ => @_) for ref($name) ? @{$name} : $name;
}

sub before {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);

    my $code = pop;

    for (@_) {
        $meta->add_before_method_modifier($_ => $code);
    }
}

sub after {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);

    my $code = pop;

    for (@_) {
        $meta->add_after_method_modifier($_ => $code);
    }
}

sub around {
    my $meta = Mouse::Meta::Class->initialize(scalar caller);

    my $code = pop;

    for (@_) {
        $meta->add_around_method_modifier($_ => $code);
    }
}

sub with {
    Mouse::Util::apply_all_roles(scalar(caller), @_);
}

our $SUPER_PACKAGE;
our $SUPER_BODY;
our @SUPER_ARGS;

sub super {
    # This check avoids a recursion loop - see
    # t/100_bugs/020_super_recursion.t
    return if defined $SUPER_PACKAGE && $SUPER_PACKAGE ne caller();
    return unless $SUPER_BODY; $SUPER_BODY->(@SUPER_ARGS);
}

sub override {
    my $meta = Mouse::Meta::Class->initialize(caller);
    my $pkg = $meta->name;

    my $name = shift;
    my $code = shift;

    my $body = $pkg->can($name)
        or confess "You cannot override '$name' because it has no super method";

    $meta->add_method($name => sub {
        local $SUPER_PACKAGE = $pkg;
        local @SUPER_ARGS = @_;
        local $SUPER_BODY = $body;

        $code->(@_);
    });
}

sub inner  { not_supported }
sub augment{ not_supported }

sub init_meta {
    shift;
    my %args = @_;

    my $class = $args{for_class}
                    or confess("Cannot call init_meta without specifying a for_class");
    my $base_class = $args{base_class} || 'Mouse::Object';
    my $metaclass  = $args{metaclass}  || 'Mouse::Meta::Class';

    confess("The Metaclass $metaclass must be a subclass of Mouse::Meta::Class.")
            unless $metaclass->isa('Mouse::Meta::Class');

    # make a subtype for each Mouse class
    Mouse::Util::TypeConstraints::class_type($class)
        unless Mouse::Util::TypeConstraints::find_type_constraint($class);

    my $meta = $metaclass->initialize($class);

    $meta->add_method(meta => sub{
        return $metaclass->initialize(ref($_[0]) || $_[0]);
    });

    $meta->superclasses($base_class)
        unless $meta->superclasses;

    return $meta;
}

sub import {
    my $class = shift;

    strict->import;
    warnings->import;

    my $opts = do {
        if (ref($_[0]) && ref($_[0]) eq 'HASH') {
            shift @_;
        } else {
            +{ };
        }
    };
    my $level = delete $opts->{into_level};
       $level = 0 unless defined $level;
    my $caller = caller($level);

    # we should never export to main
    if ($caller eq 'main') {
        warn qq{$class does not export its sugar to the 'main' package.\n};
        return;
    }

    $class->init_meta(
        for_class  => $caller,
    );

    if (@_) {
        __PACKAGE__->export_to_level( $level+1, $class, @_);
    } else {
        # shortcut for the common case of no type character
        no strict 'refs';
        for my $keyword (@EXPORT) {
            *{ $caller . '::' . $keyword } = *{__PACKAGE__ . '::' . $keyword};
        }
    }
}

sub unimport {
    my $caller = caller;

    my $stash = do{
        no strict 'refs';
        \%{$caller . '::'}
    };

    for my $keyword (@EXPORT) {
        my $code;
        if(exists $is_removable{$keyword}
            && ($code = $caller->can($keyword))
            && get_code_package($code) eq __PACKAGE__){

            delete $stash->{$keyword};
        }
    }
}

1;

__END__

#line 486

