@echo off

buildr compile && "C:\Program Files\Java\jdk1.6.0_10\bin\java" -Xmx1024m -Xms256m -server -cp "C:\Program Files\Scala\lib\scala-library.jar;target\classes" VectorPerfTest
