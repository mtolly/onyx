#!/usr/bin/env ruby

require 'yaml'

# Hastily ported line for line from the Haskell.
def load_yaml_tree(yaml)
  def stringOrStrings(v)
    case v
    when String
      return v
    when Array
      return v if v.all? { |el| el.is_a? String }
    end
    throw "Not a string or list of strings: #{v}"
  end
  def go(v, dir)
    case v
    when Hash
      go_pairs({}, v.each_pair.to_a, dir)
    when Array
      v.map { |el| go(el, dir) }
    else
      v
    end
  end
  def go_pairs(obj, pairs, dir)
    return obj if pairs.empty?
    k, v = pairs[0]
    rest = pairs[1..-1]
    if k == 'file-include'
      e = stringOrStrings(v)
      files = e.is_a?(String) ? [e] : e
      vs = files.map { |file| load_yaml_tree("#{dir}/#{file}") }
      vs.each do |loaded|
        obj = loaded.merge(obj) # this makes obj's values take precedence
      end
      go_pairs(obj, rest, dir)
    elsif k.start_with? 'file-'
      e = stringOrStrings(v)
      v = e.is_a?(String) ? "#{dir}/#{e}" : e.map { |s| "#{dir}/#{s}" }
      obj[k] = v
      go_pairs(obj, rest, dir)
    else
      obj[k] = go(v, dir)
      go_pairs(obj, rest, dir)
    end
  end
  go(YAML.load_file(yaml), File.dirname(yaml))
end
