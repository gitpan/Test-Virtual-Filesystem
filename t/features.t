use warnings;
use strict;
use File::Temp qw();
use Test::More;
use Test::Builder;

{
   package Test::Virtual::Filesystem::_Features;
   use warnings;
   use strict;
   use Test::More;
   use base 'Test::Virtual::Filesystem';

   sub _feature_test : Test(1) : Features('features_test') {
      my ($self) = @_;
      pass('features_test');
      return;
   }

   ## debugging for Test::Class v0.24, RT#30836
   #use Data::Dumper; print STDERR Dumper(Test::Class->_test_info);
}

{
   local $ENV{TEST_METHOD} = '_feature_test';
   plan tests => Test::Virtual::Filesystem::_Features->expected_tests(+1);
   my $tmpdir = File::Temp::tempdir('filesys_test_XXXX', CLEANUP => 1, TMPDIR => 1);
   diag('You should see a "SKIP" test below.  This is just testing that SKIP tests work');
   Test::Virtual::Filesystem::_Features->new({mountdir => $tmpdir})->runtests;
}

#use Data::Dumper; diag Dumper($_) for Test::Builder->new->details;
my ($result) = grep {$_->{name} eq 'features_test'
                         || $_->{reason} =~ m/features_test/xms} Test::Builder->new->details;
is($result && $result->{type}, 'skip', 'got a SKIP result');

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
