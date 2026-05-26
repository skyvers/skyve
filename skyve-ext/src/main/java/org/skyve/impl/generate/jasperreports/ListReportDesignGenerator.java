package org.skyve.impl.generate.jasperreports;

/**
 * Generates a JasperReports design for a Skyve list (query) report, producing
 * one column per selected query attribute.
 */
public class ListReportDesignGenerator extends ReportDesignGenerator {
    @Override
    protected ListReportDesignGenerator getSubreportGenerator() {
        throw new UnsupportedOperationException("Subreports are not supported in list reports.");
    }
}
