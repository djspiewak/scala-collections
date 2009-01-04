require 'buildr/scala'
#require 'buildr/cobertura'

repositories.remote << 'http://www.ibiblio.org/maven2'
repositories.remote << 'http://scala-tools.org/repo-releases'

desc 'A few collections classes for fun and profit'
define 'collection' do
  project.version = '0.1.0'
  project.group = 'com.codecommit'
 
  test.using :specs
  
  package :jar
end
