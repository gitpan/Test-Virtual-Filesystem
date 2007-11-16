use Module::Build;

Module::Build->new(
   module_name => 'Test::Virtual::Filesystem',
   dist_author => 'Chris Dolan <cdolan@cpan.org>',
   license => 'perl',
   requires => {
      'perl' => '5.6.0',
      'Test::Class' => '0.24',
      'Test::More' => 0.62,  # needs to be v0.62 for Test::Memory::Cycle v1.02
      'Readonly' => 0,
      'Class::Accessor::Fast' => 0,
   },
   recommends => {
      # Optional for testing extended attributes
      'File::ExtAttr' => 1,

      # Optional in tests, skipped if not present
      'Test::Pod' => 0,
      'Test::Pod::Coverage' => 0,
      'Test::Memory::Cycle' => '1.02',
   },
   create_readme => 1,
   create_makefile_pl => 'traditional',
)->create_build_script;
