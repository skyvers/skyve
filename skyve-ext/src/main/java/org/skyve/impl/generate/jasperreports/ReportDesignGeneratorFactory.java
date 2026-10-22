package org.skyve.impl.generate.jasperreports;

import jakarta.annotation.Nonnull;

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
    public static @Nonnull ReportDesignGenerator getGeneratorForDesign(@Nonnull DesignSpecification designSpecification) {
        assert(designSpecification.getDefinitionSource() != null);

        final DesignSpecification.DefinitionSource definitionSource = designSpecification.getDefinitionSource();
        if (definitionSource == null) {
            throw new IllegalStateException("Report design " + designSpecification.getName() + " has no definition source");
        }
        switch (definitionSource) {
            case document:
                return new DocumentReportDesignGenerator();
            case view:
                return new ViewReportDesignGenerator();
            case query:
                return new QueryReportDesignGenerator();
            case list:
                return new ListReportDesignGenerator();
            default:
            	throw new IllegalStateException(definitionSource + " is not catered for");
        }
    }
}
