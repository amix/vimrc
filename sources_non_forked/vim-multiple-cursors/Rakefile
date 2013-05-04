require 'rspec/core/rake_task'

RSpec::Core::RakeTask.new(:spec) do |t|
  t.pattern = 'spec/multiple_cursors_spec.rb'
end

RSpec::Core::RakeTask.new(:benchmark) do |t|
  t.pattern = 'spec/benchmark_spec.rb'
end

task :default => :spec
