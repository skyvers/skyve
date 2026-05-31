package org.skyve.impl.generate.jasperreports;

/**
 * Factory that selects and instantiates the appropriate {@link ReportDesignGenerator}
 * subclass based on the report parameters supplied.
 */
public class ReportDesignGeneratorFactory {
    /**
     * Selects a report design generator based on the design definition source.
     *
     * @param designSpecification The design to generate.
     * @return A generator implementation compatible with the definition source.
     * @throws IllegalStateException If the definition source is not supported.
     */
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
