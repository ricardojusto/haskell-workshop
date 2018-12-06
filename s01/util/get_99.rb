#!/usr/bin/env ruby

require 'nokogiri'
require 'open-uri'

# https://stackoverflow.com/questions/1556028/how-do-i-do-a-regex-search-in-nokogiri-for-text-that-matches-a-certain-beginning
Nokogiri::XML::Node.send(:define_method, 'xpath_regex') { |*args|
  xpath = args[0]
  rgxp = /\/([a-z]+)\[@([a-z\-]+)~=\/(.*?)\/\]/
  xpath.gsub!(rgxp) do |s|
    m = s.match(rgxp)
    "/#{m[1]}[regex(.,'#{m[2]}','#{m[3]}')]"
  end
  self.xpath(xpath, Class.new {
    def regex node_set, attr, regex
      node_set.find_all { |node| node[attr] =~ /#{regex}/ }
    end
  }.new)
}

# Nokogiri::HTML document from 99 haskell problems
wiki = "https://wiki.haskell.org"
h99p = "/H-99:_Ninety-Nine_Haskell_Problems"
doc = Nokogiri::HTML(open(wiki+h99p));

# Search problem sets (pages)
doc.xpath_regex("//a[@href~=/_to_/]").each do |row| 
  chunk = row.attribute('href').text

  problems = Hash.new("")
  problem  = ""
  examples = {}

  page = Nokogiri::HTML(open(wiki+chunk));
  page.xpath("//div[@id='mw-content-text']").first.children.each do |elem|
    case elem.name
    when 'h2'  # Problem X
      problem = elem.content.sub(/\s*\d\s*/, '')
    when 'p'   # Problem description or "Example:"
      if elem.content !~ /Example:|Solutions/
        problems[problem]+=elem.content+"\n" if problems[problem]
      end
    when 'div' # pre
      examples[problem]=elem.content
      problems[problem]+=elem.content+"\n" if problems[problem]
    end
  end

  output_dir='output/'
  Dir.mkdir(output_dir) unless File.exists?(output_dir)
  filename=output_dir+chunk.gsub(/.*\//,"")+".hs"
  File.open(filename, 'w') do |f|
    problems.each do |k, v|
      f.puts '{-|'
      f.puts k unless k==''
      f.write v
      f.puts '-}'
      f.puts k.downcase.strip.gsub(' ', '_')+" :: Int -> Int"
      f.puts k.downcase.strip.gsub(' ', '_')+" n = n"
    end
  end
  filename=output_dir+chunk.gsub(/.*\//,"")+"_test.hs"
  File.open(filename, 'w') do |f|
    examples.each do |k, v|
      f.puts k.downcase.strip.gsub(' ', '_')
      f.puts v
    end
  end
end

