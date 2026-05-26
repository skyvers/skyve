package org.skyve.impl.generate.jasperreports;

/**
 * Factory that selects and instantiates the appropriate {@link ReportDesignGenerator}
 * subclass based on the report parameters supplied.
 */
public class ReportDesignGeneratorFactory {
    public static ReportDesignGenerator getGeneratorForDesign(DesignSpecification designSpecification) {
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
            default:
            	throw new IllegalStateException(designSpecification.getDefinitionSource() + " is not catered for");
        }
    }
}
