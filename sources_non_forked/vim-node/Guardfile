guard :minitest, :all_on_start => false, :cli => ENV["TEST_OPTS"] do
  watch(%r(^(.*)\.vim$)) {|m| "test/#{m[1]}_test.rb" }
  watch(%r(^([^/]+)/[^/]+\.vim$)) {|m| "test/#{m[1]}_test.rb" }
  watch(%r(^([^/]+)/[^/]+/(.*)\.vim$)) {|m| "test/#{m[1]}/#{m[2]}_test.rb" }

  watch(%r(^test/(.*)\/?_test\.rb))
  watch(%r(^test/helper\.rb)) { "test" }
end
