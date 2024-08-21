package org.skyve.impl.archive.support;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.apache.commons.lang3.StringUtils.left;
import static org.apache.commons.lang3.StringUtils.right;

import java.nio.charset.Charset;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;

public class ArchiveUtils {

    public static Charset ARCHIVE_CHARSET = UTF_8;

    /**
     * How much of the start and end of a string to include in the excerpt.
     */
    private static final int EXCERPT_LENGTH = 100;

    public static final String ARCHIVE_FILE_SUFFIX = ".archive";

    /**
     * Create an excerpt of the provided string, showing the start and end of it,
     * seperated by <em>[...]</em>. Returns the original string if it's not long
     * enough to warrant an excerpt. Returns the String <em>"null"</em> for a
     * null lineValue.
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


    public static Document getDocument(String module, String document) {
        Customer customer = CORE.getUser()
                                .getCustomer();

        return customer.getModule(module)
                       .getDocument(customer, document);
    }

}
