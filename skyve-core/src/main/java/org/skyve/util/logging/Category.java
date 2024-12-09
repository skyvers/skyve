package org.skyve.util.logging;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    LEGACY("legacy");

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

    public Logger logger() {
        return LoggerFactory.getLogger(name);
    }

}
