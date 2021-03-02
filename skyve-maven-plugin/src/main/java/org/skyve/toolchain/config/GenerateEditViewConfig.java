package org.skyve.toolchain.config;

import org.apache.maven.plugins.annotations.Parameter;

public class GenerateEditViewConfig {
    /**
     * Customer name.
     */
    @Parameter(required = true)
    private String customer;

    /**
     * Module name.
     */
    @Parameter(required = true)
    private String module;

    /**
     * Dcoument name.
     */
    @Parameter(required = true)
    private String document;

    /**
     * Customer overriden view.
     */
    @Parameter(required = true, defaultValue = "false")
    private boolean customerOverriden = false;

    /**
     * UXUI overridden view name.
     */
    @Parameter()
    private String overridenViewName;

    public String getCustomer() {
        return customer;
    }

    public void setCustomer(String customer) {
        this.customer = customer;
    }

    public String getModule() {
        return module;
    }

    public void setModule(String module) {
        this.module = module;
    }

    public String getDocument() {
        return document;
    }

    public void setDocument(String document) {
        this.document = document;
    }

    public boolean isCustomerOverriden() {
        return customerOverriden;
    }

    public void setCustomerOverriden(boolean customerOverriden) {
        this.customerOverriden = customerOverriden;
    }

    public String getOverridenViewName() {
        return overridenViewName;
    }

    public void setOverridenViewName(String overridenViewName) {
        this.overridenViewName = overridenViewName;
    }
}
