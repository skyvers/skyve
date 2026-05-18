package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.jasperreports.DesignSpecification.DefinitionSource;

@SuppressWarnings("static-method")
class ReportDesignGeneratorFactoryTest {

	private DesignSpecification specFor(DefinitionSource source) {
		DesignSpecification spec = new DesignSpecification();
		spec.setModuleName("test");
		spec.setDocumentName("AllAttributesPersistent");
		spec.setDefinitionSource(source);
		return spec;
	}

	@Test
	void returnsDocumentGeneratorForDocument() {
		ReportDesignGenerator gen = ReportDesignGeneratorFactory.getGeneratorForDesign(specFor(DefinitionSource.document));
		assertThat(gen, instanceOf(DocumentReportDesignGenerator.class));
	}

	@Test
	void returnsViewGeneratorForView() {
		ReportDesignGenerator gen = ReportDesignGeneratorFactory.getGeneratorForDesign(specFor(DefinitionSource.view));
		assertThat(gen, instanceOf(ViewReportDesignGenerator.class));
	}

	@Test
	void returnsQueryGeneratorForQuery() {
		ReportDesignGenerator gen = ReportDesignGeneratorFactory.getGeneratorForDesign(specFor(DefinitionSource.query));
		assertThat(gen, instanceOf(QueryReportDesignGenerator.class));
	}

	@Test
	void returnsListGeneratorForList() {
		ReportDesignGenerator gen = ReportDesignGeneratorFactory.getGeneratorForDesign(specFor(DefinitionSource.list));
		assertThat(gen, instanceOf(ListReportDesignGenerator.class));
	}
}
