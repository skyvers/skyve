package org.skyve.impl.generate.jasperreports;

import jakarta.annotation.Nonnull;

/**
 * Generates a JasperReports design for a Skyve list (query) report, producing
 * one column per selected query attribute.
 */
public class ListReportDesignGenerator extends ReportDesignGenerator {
    /**
     * Indicates that list reports do not support collection subreport generation.
     *
     * @return Never returns normally.
     * @throws UnsupportedOperationException Always.
     */
    @Override
    protected @Nonnull ListReportDesignGenerator getSubreportGenerator() {
        throw new UnsupportedOperationException("Subreports are not supported in list reports.");
    }
}
