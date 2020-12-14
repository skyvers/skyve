package org.skyve.impl.generate.jasperreports;

public class ReportDesignGeneratorFactory {
    public ReportDesignGenerator getGeneratorForDesign(DesignSpecification designSpecification) {
        assert(designSpecification.getDefinitionSource() != null);

        switch (designSpecification.getDefinitionSource()) {
            case document:
                return new DocumentReportDesignGenerator();
            case view:
                return new ViewReportDesignGenerator();
            case query:
                return new QueryReportDesignGenerator();
            case list:
                return new ListReportDesignGenerator();
        }

        throw new IllegalArgumentException("Unknown design definition source.");
    }
}
