package modules.admin.Audit.job.support;

import static org.apache.commons.lang3.StringUtils.left;
import static org.apache.commons.lang3.StringUtils.right;

import java.nio.file.Path;

import org.skyve.util.Util;

public class ArchiveUtils {

    /**
     * How much of the start and end of a string to include in the excerpt.
     */
    private static final int EXCERPT_LENGTH = 100;

    /**
     * Directory within the archive directory (itself inside the content dir) where
     * the lucene index lives.
     */
    private static final String INDEX_DIR = "index";

    /**
     * Create an excerpt of the provided string, showing the start and end of it, seperated by <em>[...]</em>. Returns the original
     * string if it's not long enough to warrant an excerpt. Returns the String <em>"null"</em> for a null lineValue;
     * 
     * @param line Line/String to excerpt
     * @return
     */
    public static String excerptLine(String line) {
        if (line == null || line.length() <= (2 * EXCERPT_LENGTH) + 20) {
            return String.valueOf(line);
        }

        return left(line, EXCERPT_LENGTH) + " … " + right(line, EXCERPT_LENGTH);
    }

    /**
     * Get the path of the archive index. The default is <em>archive/index</em>
     * within the content directory.
     * 
     * @return Path to the archive index
     */
    public static Path getIndexPath() {

        return Util.getArchiveDirectory()
                   .resolve(INDEX_DIR);
    }
}
