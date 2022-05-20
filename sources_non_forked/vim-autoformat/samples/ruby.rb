require 'inifile'

# Reads settings from init file
class Settings
  attr_reader :jusername, :jpassword, :jurl

  def initialize(path)
    settings = read(path)
    parse(settings)
  end

  private

  def read(path)
    settings = IniFile.load(path)
    fail "File #{path} not found!" unless settings
    settings
  end

  def parse(settings)
    jira = settings['jira']
    fail "Init file hasn't [jira] section!" unless jira

    @jusername = jira['username']
    @jpassword = jira['password']
    @jurl      = jira['url']

    fail "Init file hasn't username option!" unless jusername
    fail "Init file hasn't password option!" unless jpassword
    fail "Init file hasn't url option!" unless jurl
  end
end

