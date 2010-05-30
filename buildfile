ENV['SCALA_HOME'] = ENV['SCALA28_HOME']

require 'buildr/scala'
#require 'buildr/java/cobertura'

repositories.remote << 'http://repo1.maven.org/maven2'
repositories.remote << 'http://scala-tools.org/repo-snapshots'

Buildr::Scala::Specs.dependencies.delete_if do |str| 
  str =~ /specs/ ||
    str =~ /scalacheck/
end

Buildr::Scala::Specs.dependencies << 'org.scala-tools.testing:specs_2.8.0.RC3:jar:1.6.5-SNAPSHOT'
Buildr::Scala::Specs.dependencies << 'org.scala-tools.testing:scalacheck_2.8.0.RC3:jar:1.7'


desc 'A few collections classes for fun and profit'
define 'collection' do
  project.version = '0.1.0'
  project.group = 'com.codecommit'

  test.using :specs
  
  package :jar
end
