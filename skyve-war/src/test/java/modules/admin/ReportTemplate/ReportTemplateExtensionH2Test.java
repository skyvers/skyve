package modules.admin.ReportTemplate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;

import modules.admin.domain.ReportTemplate;
import util.AbstractH2Test;

/**
 * Tests for ReportTemplateExtension methods that require H2 context.
 */
@SuppressWarnings("static-method")
public class ReportTemplateExtensionH2Test extends AbstractH2Test {

	@Test
	void generateInitialFreemarkerTemplatePopulatesTemplate() throws Exception {
		ReportTemplateExtension bean = ReportTemplate.newInstance();
		bean.setGenerateModuleName("admin");
		bean.setGenerateDocumentName("User");
		bean.generateInitialFreemarkerTemplate();
		assertNotNull(bean.getTemplate());
		assertFalse(bean.getTemplate().isEmpty());
		assertTrue(bean.getTemplate().contains("headerPortrait.ftlh"));
	}

	@Test
	void generateInitialDatasetAddsTwoDatasets() throws Exception {
		ReportTemplateExtension bean = ReportTemplate.newInstance();
		bean.setGenerateModuleName("admin");
		bean.setGenerateDocumentName("User");
		bean.setName("My Report");
		bean.generateInitialDataset();
		assertEquals(2, bean.getDatasets().size());
		boolean hasBizQL = bean.getDatasets().stream()
				.anyMatch(d -> d.getDatasetType() == DatasetType.bizQL);
		boolean hasConstant = bean.getDatasets().stream()
				.anyMatch(d -> d.getDatasetType() == DatasetType.constant);
		assertTrue(hasBizQL);
		assertTrue(hasConstant);
	}
}
