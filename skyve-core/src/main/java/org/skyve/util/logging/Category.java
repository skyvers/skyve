package org.skyve.util.logging;

import org.slf4j.Logger;

/**
 * Logging categories for use within the Skyve framework. Typically associated with
 * the trace flags set through the Admin > Instrumentation UI, or the application
 * config JSON. 
 * <p>
 * Do not use these outside of framework code.
 */
public enum Category {
    XML("xml"),
    HTTP("http"),
    QUERY("query"),
    COMMAND("command"),
    FACES("faces"),
    CONTENT("content"),
    SECURITY("security"),
    BIZLET("bizlet"),
    DIRTY("dirty"),
    /**
     * This category exists to (temporarily) unify the UtilImpl's logger with these
     * other Categories.
     * 
     * @deprecated This category will be deleted along with the Util/UtilImpl LOGGER fields
     *             in a subsequent release.
     */
    @Deprecated
    LEGACY("legacy");

    /**
     * This logging category prefix is shared by all of these categories. 
     * It starts with "SKYVE" to maintain some backwards compatibility with
     * any existing logging configurations that may be in use (ie: any 
     * handlers which filter on "SKYVE" will still capture all of these 
     * categories).
     */
    private static final String CATEGORY_PREFIX = "SKYVE.framework.";

    private final String name;

    Category(String name) {
        this.name = new StringBuilder().append(CATEGORY_PREFIX)
                                       .append(name)
                                       .toString();
    }

    public String getName() {
        return name;
    }

    /**
     * Returns a sanitising {@link SkyveLoggerFactory} named for this category.
     *
     * <p>The returned logger strips CR, LF, and other ASCII control characters
     * from messages and {@link String} arguments before passing them to the
     * underlying SLF4J backend, preventing log injection attacks.
     *
     * @return a sanitising {@link Logger}; never {@code null}
     */
    public Logger logger() {
        return SkyveLoggerFactory.getLogger(this);
    }

}
