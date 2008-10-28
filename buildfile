require 'buildr/scala'

repositories.remote << 'http://www.ibiblio.org/maven2'
repositories.remote << 'http://scala-tools.org/repo-releases'

# SPECS_VERSION = '1.3.1'
# 
# SPECS_URL = 'http://specs.googlecode.com/files'
# SPECS = "com.googlecode.specs:specs:jar:#{SPECS_VERSION}"
# 
# SCALACHECK_VERSION = '1.3'
# SCALACHECK_URL = 'http://scalacheck.googlecode.com/files'
# SCALACHECK = "com.googlecode.scalacheck:scalacheck:jar:#{SCALACHECK_VERSION}"
# 
# download artifact(SPECS) => "#{SPECS_URL}/specs-#{SPECS_VERSION}.jar"
# download artifact(SCALACHECK) => "#{SCALACHECK_URL}/ScalaCheck-#{SCALACHECK_VERSION}.jar"

desc 'A few collections classes for fun and profit'
define 'collection' do
  project.version = '0.1.0'
  project.group = 'com.codecommit'
  
  package :jar
end
