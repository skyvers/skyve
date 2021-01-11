package org.skyve.toolchain.config;

import org.apache.maven.plugins.annotations.Parameter;

public class NewDocumentConfig {
    /**
     * Default module.
     */
    @Parameter()
    private String defaultModule;

    public String getDefaultModule() {
        return defaultModule;
    }

    public void setDefaultModule(String defaultModule) {
        this.defaultModule = defaultModule;
    }
}
