use warnings;
use strict;
use File::Temp qw();
use Test::More;
use Test::Builder;

use Test::Virtual::Filesystem;

{
   local $ENV{TEST_METHOD} = '_compatibility_test'; # pick a special test introduced after v0.01
   plan tests => Test::Virtual::Filesystem->expected_tests(+1);
   my $tmpdir = File::Temp::tempdir('filesys_test_XXXX', CLEANUP => 1, TMPDIR => 1);
   diag('You should see a "TODO" test below.  This is just testing that TODO tests work');
   Test::Virtual::Filesystem->new({mountdir => $tmpdir, compatible => '0.01'})->runtests;
}

#use Data::Dumper; diag Dumper($_) for Test::Builder->new->details;
my ($result) = grep {$_->{name} eq 'compatibility_test'} Test::Builder->new->details;
is($result && $result->{type}, 'todo', 'got a TODO result');

__END__

# Local Variables:
#   mode: perl
#   cperl-indent-level: 3
#   perl-indent-level: 3
#   fill-column: 78
#   indent-tabs-mode: nil
#   c-indentation-style: bsd
# End:
# ex: set ts=8 sts=4 sw=4 tw=78 ft=perl expandtab :
