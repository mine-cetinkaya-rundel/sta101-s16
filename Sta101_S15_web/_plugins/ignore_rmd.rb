# so that .rmd files are ignored in build and their yaml doesn't get stripped


class RmdCopyer < Jekyll::Generator
    RMD_FOLDER = 'post/rmd'
    RMD_OUTPUT_DIR = ''
    def generate(site)
        FileUtils.cp_r(Jekyll.sanitized_path(site.source, RMD_FOLDER), Jekyll.sanitized_path(site.dest, RMD_OUTPUT_DIR))
        site.keep_files << RMD_OUTPUT_DIR unless site.keep_files.include?(RMD_OUTPUT_DIR)
    end
end