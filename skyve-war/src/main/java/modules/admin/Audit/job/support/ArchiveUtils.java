package modules.admin.Audit.job.support;

import static org.apache.commons.lang3.StringUtils.left;
import static org.apache.commons.lang3.StringUtils.right;

public class ArchiveUtils {

    /**
     * How much of a string to include in a excerpt at the start AND end of the line
     */
    private static final int HALF_EXCERPT_LENGTH = 100;

    /**
     * Create an excerpt of the provided string, showing the start and end of it, seperated by <em>[...]</em>. Returns the original
     * string if it's not long enough to warrant an excerpt. Returns the String <em>"null"</em> for a null lineValue;
     * 
     * @param lineValue
     * @return
     */
    public static String excerptLine(String lineValue) {
        if (lineValue == null || lineValue.length() <= 2 * HALF_EXCERPT_LENGTH) {
            return String.valueOf(lineValue);
        }

        return left(lineValue, HALF_EXCERPT_LENGTH) + "[...]" + right(lineValue, HALF_EXCERPT_LENGTH);
    }
}
