package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;

import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.test.AbstractSkyveTest;

class ReportDatasetDomainTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void moduleAndDocumentNames() {
		ReportDatasetExtension bean = new ReportDatasetExtension();
		assertEquals("admin", bean.getBizModule());
		assertEquals("ReportDataset", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void datasetNameSetAndGet() {
		ReportDatasetExtension bean = new ReportDatasetExtension();
		bean.setDatasetName("mainDataset");
		assertEquals("mainDataset", bean.getDatasetName());
	}

	@Test
	@SuppressWarnings("static-method")
	void datasetTypeDefaultIsBizQL() {
		ReportDatasetExtension bean = new ReportDatasetExtension();
		assertEquals(DatasetType.bizQL, bean.getDatasetType());
	}

	@Test
	@SuppressWarnings("static-method")
	void datasetTypeSetToSQL() {
		ReportDatasetExtension bean = new ReportDatasetExtension();
		bean.setDatasetType(DatasetType.SQL);
		assertEquals(DatasetType.SQL, bean.getDatasetType());
		assertTrue(bean.isTypeQuery());
	}

	@Test
	@SuppressWarnings("static-method")
	void datasetTypeSetToConstant() {
		ReportDatasetExtension bean = new ReportDatasetExtension();
		bean.setDatasetType(DatasetType.constant);
		assertEquals(DatasetType.constant, bean.getDatasetType());
		assertTrue(bean.isTypeConstant());
	}

	@Test
	@SuppressWarnings("static-method")
	void datasetTypeSetToClass() {
		ReportDatasetExtension bean = new ReportDatasetExtension();
		bean.setDatasetType(DatasetType.classValue);
		assertEquals(DatasetType.classValue, bean.getDatasetType());
		assertTrue(bean.isTypeClass());
	}

	@Test
	@SuppressWarnings("static-method")
	void querySetAndGet() {
		ReportDatasetExtension bean = new ReportDatasetExtension();
		bean.setQuery("select bean from {admin.User} bean");
		assertEquals("select bean from {admin.User} bean", bean.getQuery());
	}

	@Test
	@SuppressWarnings("static-method")
	void resultsSetAndGet() {
		ReportDatasetExtension bean = new ReportDatasetExtension();
		bean.setResults("[{\"row\":1}]");
		assertEquals("[{\"row\":1}]", bean.getResults());
	}

	@Test
	@SuppressWarnings("static-method")
	void isBizQLTypeWhenDefault() {
		ReportDatasetExtension bean = new ReportDatasetExtension();
		assertTrue(bean.isTypeQuery());
	}

	@Test
	@SuppressWarnings("static-method")
	void isNotConstantTypeWhenDefault() {
		ReportDatasetExtension bean = new ReportDatasetExtension();
		assertEquals(DatasetType.bizQL, bean.getDatasetType());
		assertNotNull(bean.getDatasetType());
	}
}
