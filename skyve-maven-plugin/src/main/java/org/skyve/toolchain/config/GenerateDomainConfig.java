package org.skyve.toolchain.config;

import org.apache.maven.plugins.annotations.Parameter;

public class GenerateDomainConfig {

    /**
     * Debug mode switch.
     */
    @Parameter(required = true, defaultValue = "false")
    private boolean debug = false;

    /**
     * Debug mode switch.
     */
    @Parameter(required = true, defaultValue = "false")
    private boolean multiTenant = false;

    /**
     * Dialect options.
     */
    @Parameter(required = true, defaultValue = "H2")
    private String dialect = "H2";

    /**
     * Comma separated list of modules to exclude.
     */
    @Parameter()
    private String excludedModules = "";

    public boolean isDebug() {
        return debug;
    }

    public void setDebug(boolean debug) {
        this.debug = debug;
    }

    public boolean isMultiTenant() {
        return multiTenant;
    }

    public void setMultiTenant(boolean multiTenant) {
        this.multiTenant = multiTenant;
    }

    public String getDialect() {
        return dialect;
    }

    public void setDialect(String dialect) {
        this.dialect = dialect;
    }

    public String getExcludedModules() {
        return excludedModules;
    }

    public void setExcludedModules(String excludedModules) {
        this.excludedModules = excludedModules;
    }
}
