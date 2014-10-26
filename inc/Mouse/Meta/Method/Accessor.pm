#line 1
package Mouse::Meta::Method::Accessor;
use strict;
use warnings;
use Scalar::Util qw(blessed);

sub _install_accessor{
    my (undef, $attribute, $method_name, $class, $type) = @_;

    my $name          = $attribute->name;
    my $default       = $attribute->default;
    my $constraint    = $attribute->type_constraint;
    my $builder       = $attribute->builder;
    my $trigger       = $attribute->trigger;
    my $is_weak       = $attribute->is_weak_ref;
    my $should_deref  = $attribute->should_auto_deref;
    my $should_coerce = $attribute->should_coerce;

    my $compiled_type_constraint    = $constraint ? $constraint->{_compiled_type_constraint} : undef;

    my $self  = '$_[0]';
    my $key   = sprintf q{"%s"}, quotemeta $name;

    $type ||= 'accessor';

    my $accessor = 
        '#line ' . __LINE__ . ' "' . __FILE__ . "\"\n" .
        "sub {\n";
    if ($type eq 'accessor' || $type eq 'writer') {
        if($type eq 'accessor'){
            $accessor .= 
                '#line ' . __LINE__ . ' "' . __FILE__ . "\"\n" .
                'if (scalar(@_) >= 2) {' . "\n";
        }
        else{ # writer
            $accessor .= 
                '#line ' . __LINE__ . ' "' . __FILE__ . "\"\n" .
                'if(@_ < 2){ Carp::confess("Not enough arguments for writer '.$method_name.'") }'.
                '{' . "\n";
        }
                
        my $value = '$_[1]';

        if ($constraint) {
            if ($should_coerce) {
                $accessor .=
                    "\n".
                    '#line ' . __LINE__ . ' "' . __FILE__ . "\"\n" .
                    'my $val = Mouse::Util::TypeConstraints->typecast_constraints("'.$attribute->associated_class->name.'", $attribute->{type_constraint}, '.$value.');';
                $value = '$val';
            }
            if ($compiled_type_constraint) {
                $accessor .= 
                    "\n".
                    '#line ' . __LINE__ . ' "' . __FILE__ . "\"\n" .
                    'unless ($compiled_type_constraint->('.$value.')) {
                        $attribute->verify_type_constraint_error($name, '.$value.', $attribute->{type_constraint});
                    }' . "\n";
            } else {
                $accessor .= 
                    "\n".
                    '#line ' . __LINE__ . ' "' . __FILE__ . "\"\n" .
                    'unless ($constraint->check('.$value.')) {
                        $attribute->verify_type_constraint_error($name, '.$value.', $attribute->{type_constraint});
                    }' . "\n";
            }
        }

        # if there's nothing left to do for the attribute we can return during
        # this setter
        $accessor .= 'return ' if !$is_weak && !$trigger && !$should_deref;

        $accessor .= $self.'->{'.$key.'} = '.$value.';' . "\n";

        if ($is_weak) {
            $accessor .= 'Scalar::Util::weaken('.$self.'->{'.$key.'}) if ref('.$self.'->{'.$key.'});' . "\n";
        }

        if ($trigger) {
            $accessor .= '$trigger->('.$self.', '.$value.');' . "\n";
        }

        $accessor .= "}\n";
    }
    elsif($type eq 'reader') {
        $accessor .= 'Carp::confess("Cannot assign a value to a read-only accessor") if scalar(@_) >= 2;' . "\n";
    }
    else{
        $class->throw_error("Unknown accessor type '$type'");
    }

    if ($attribute->is_lazy) {
        $accessor .= $self.'->{'.$key.'} = ';

        $accessor .= $attribute->has_builder
                ? $self.'->$builder'
                    : ref($default) eq 'CODE'
                    ? '$default->('.$self.')'
                    : '$default';
        $accessor .= ' if !exists '.$self.'->{'.$key.'};' . "\n";
    }

    if ($should_deref) {
        if ($constraint->is_a_type_of('ArrayRef')) {
            $accessor .= 'if (wantarray) {
                return @{ '.$self.'->{'.$key.'} || [] };
            }';
        }
        elsif($constraint->is_a_type_of('HashRef')){
            $accessor .= 'if (wantarray) {
                return %{ '.$self.'->{'.$key.'} || {} };
            }';
        }
        else{
            $class->throw_error("Can not auto de-reference the type constraint " . $constraint->name);
        }
    }

    $accessor .= 'return '.$self.'->{'.$key."};\n}";

    #print $accessor, "\n";
    my $code = eval $accessor;
    $attribute->throw_error($@) if $@;

    $class->add_method($method_name => $code);
    return;
}

sub _install_reader{
    my $class = shift;
    $class->_install_accessor(@_, 'reader');
    return;
}

sub _install_writer{
    my $class = shift;
    $class->_install_accessor(@_, 'writer');
    return;
}


sub _install_predicate {
    my (undef, $attribute, $method_name, $class) = @_;

    my $slot = $attribute->name;

    $class->add_method($method_name => sub{
        return exists $_[0]->{$slot};
    });
    return;
}

sub _install_clearer {
    my (undef, $attribute, $method_name, $class) = @_;

    my $slot = $attribute->name;

    $class->add_method($method_name => sub{
        delete $_[0]->{$slot};
    });
    return;
}

sub _install_handles {
    my (undef, $attribute, $handles, $class) = @_;

    my $reader  = $attribute->reader || $attribute->accessor
        or $class->throw_error("You must pass a reader method for '".$attribute->name."'");

    my %handles = $attribute->_canonicalize_handles($handles);

    foreach my $handle_name (keys %handles) {
        my $method_to_call = $handles{$handle_name};

        my $code = sub {
            my $instance = shift;
            my $proxy    = $instance->$reader();

            my $error = !defined($proxy)                ? ' is not defined'
                      : ref($proxy) && !blessed($proxy) ? qq{ is not an object (got '$proxy')}
                                                        : undef;
            if ($error) {
                $instance->meta->throw_error(
                    "Cannot delegate $handle_name to $method_to_call because "
                        . "the value of "
                        . $attribute->name
                        . $error
                 );
            }
            $proxy->$method_to_call(@_);
        };
        $class->add_method($handle_name => $code);
    }
    return;
}


1;
