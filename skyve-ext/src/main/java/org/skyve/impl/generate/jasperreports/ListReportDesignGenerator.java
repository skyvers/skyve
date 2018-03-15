package org.skyve.impl.generate.jasperreports;

public class ListReportDesignGenerator extends ReportDesignGenerator {
    @Override
    protected ListReportDesignGenerator getSubreportGenerator() {
        throw new UnsupportedOperationException("Subreports are not supported in list reports.");
    }
}
