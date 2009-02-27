require 'buildr/scala'
#require 'buildr/java/cobertura'
require 'buildr/groovy'

repositories.remote << 'http://repo1.maven.org/maven2/'

desc 'A few collections classes for fun and profit'
define 'collection' do
  project.version = '0.1.0'
  project.group = 'com.codecommit'

  test.using :specs
  
  package :jar
end
