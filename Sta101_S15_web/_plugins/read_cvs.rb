#
#
# Add data read from _csv directory to site.data
#
# For each csv file <<name>> found in _csv/, this plugin adds the
# following information:
#
# * site.data.<<name>>.rows (number of rows)
# * site.data.<<name>>.cols (number of columns)
#
# * site.data.<<name>>.keys (Array containing the headers of the csv file)
# * site.data.<<name>>.content (array of arrays: content of the csv file)
# * site.data.<<name>>.content_hash (array of hashes: content of the csv file)
#
# The content is available similar to @_data@
#
# Example
#
# Suppose you have a my_table.csv file in the _csv directory,
# with the following content:
#
#  h1,h2,h3
#  a,b,c
#  1,2,3
#
# You can build an HTML representation of the table with the following code:
#
# <table>
# <thead>
# <tr>
# {% for header in site.data.my_table.keys %}
#   <td>{{header}}</td>
# {% endfor %}
# </tr>
# </thead>
#
# <tbody>
# {% for row in site.data.my_table.content %}
# <tr>
#   {% for column in row %}
#      <td>{{column}}</td>
#   {% endfor %}
# </tr>
# {% endfor %}
# </tbody>
# </table>
#
# content_hash can be used to access the content using the header keys.
# For instance:
#
# {% for row in site.data.my_table.content_hash %}
# <tr>
#   <td>{{row.h1}}</td>
#   <td>{{row.h3}}</td>
# </tr>
# {% endfor %}
# </tbody>
# </table>
 

module CSVDataReader
  require 'csv'

  class Generator < Jekyll::Generator
    def generate(site)
      #dir = config['csv_data_source'] || '_csv'
      dir = "_csv"
      base = File.join(site.source, dir)
      return unless File.directory?(base) && (!site.safe || !File.symlink?(base))

      entries = Dir.chdir(base) { Dir['*.csv'] }
      entries.delete_if { |e| File.directory?(File.join(base, e)) }

      entries.each do |entry|
        path = File.join(site.source, dir, entry)
        next if File.symlink?(path) && site.safe

        key = sanitize_filename(File.basename(entry, '.*'))
        file_data = CSV.read(path, :headers => true)

        data = Hash.new        
        data['content'] = file_data.to_a[1..-1]
        data['content_hash'] = file_data.map(&:to_hash)
        data['keys'] = file_data.headers
        data['rows'] = data['content'].size
        data['cols'] = file_data.headers.size

        csv_data = Hash.new
        csv_data[key] = data

        site.data.merge!(csv_data)
      end
    end
   
    private

    # copied from Jekyll
    def sanitize_filename(name)
      name = name.gsub(/[^\w\s_-]+/, '')
      name = name.gsub(/(^|\b\s)\s+($|\s?\b)/, '\\1\\2')
      name = name.gsub(/\s+/, '_')
    end
  end
end
