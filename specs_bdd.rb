require 'buildr/java/tests'

module Buildr

  # Specs is a Scala based BDD framework.
  # To use in your project:
  #
  #   test.using :specs
  # 
  # This framework will search in your project for:
  #   src/spec/scala/**/*Spec*.scala
  #
  # Support the following options:
  # * :nostacktrace -- set to true if you want to exclude stacktraces from the console. Default = false
  class Specs < TestFramework::Base
    include TestFramework::JavaBDD
    self.lang = :scala
    
    REQUIRES = ['com.googlecode.specs:specs:jar:1.3.1', 
                'com.googlecode.scalacheck:scalacheck:jar:1.3',
                'cglib:cglib:jar:2.1_3',
                'org.hamcrest:hamcrest-all:jar:1.0',
                'org.objenesis:objenesis:jar:1.0',
                'asm:asm:jar:1.5.3',
                'org.jmock:jmock:jar:2.4.0']
    TESTS_PATTERN = [ /.*Spec.*.scala$/ ]
    OPTIONS = [:properties, :java_args]

    def self.applies_to?(project) #:nodoc:
      %w{
        **/*.scala
      }.any? { |glob| !Dir[project.path_to(:source, bdd_dir, lang, glob)].empty? }
    end
    def tests(dependencies) #:nodoc:
      Dir[task.project.path_to(:source, bdd_dir, lang, "**/*.scala")].
        select { |name| TESTS_PATTERN.any? { |pat| pat === name } }
    end

    def run(tests, dependencies) #:nodoc:
      options = { :nostacktrace => false }.merge(self.options).only(*OPTIONS)
      
      nostacktrace = (options[:nostacktrace]) ? "-ns" : ""
      
      cmd_options = { :properties => options[:properties],
                      :java_args => options[:java_args],
                      :classpath => dependencies}

      tests.inject([]) do |passed, test|
        name = test.sub(/.*?scala[\/\\]/, '').pathmap('%X').gsub("/", ".")
        begin
          Java::Commands.java [name], nostacktrace, cmd_options
        rescue => e
		print e.message
          passed
        else
          passed << test
        end
      end
    end
    
  end # Specs

end

Buildr::TestFramework << Buildr::Specs
