#!/usr/bin/env rake

task :ci => [:dump, :test]

task :dump do
  sh 'vim --version'
end

task :test do
  sh 'bundle exec vim-flavor test'
end
