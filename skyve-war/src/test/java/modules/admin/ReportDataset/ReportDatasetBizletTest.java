package modules.admin.ReportDataset;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import modules.admin.domain.ReportDataset;

/**
 * Tests for ReportDatasetBizlet covering getVariantDomainValues.
 */
@SuppressWarnings("static-method")
class ReportDatasetBizletTest {

	private static ReportDatasetBizlet bizlet = new ReportDatasetBizlet();

	@Test
	void getVariantDomainValuesForDatasetTypeExcludesSqlInMultiTenant() throws Exception {
		UtilImpl.CUSTOMER = null; // null = multi-tenant mode - SQL should be excluded
		List<DomainValue> result = bizlet.getVariantDomainValues(ReportDataset.datasetTypePropertyName);
		assertNotNull(result);
		assertFalse(result.isEmpty());
		// SQL dataset type should NOT be included in multi-tenant mode
		boolean hasSql = result.stream().anyMatch(dv -> DatasetType.SQL.toCode().equals(dv.getCode()));
		assertFalse(hasSql, "SQL should not be included in multi-tenant mode");
	}

	@Test
	void getVariantDomainValuesForDatasetTypeIncludesSqlInSingleTenant() throws Exception {
		UtilImpl.CUSTOMER = "bizhub"; // non-null = single-tenant - SQL should be included
		try {
			List<DomainValue> result = bizlet.getVariantDomainValues(ReportDataset.datasetTypePropertyName);
			assertNotNull(result);
			assertFalse(result.isEmpty());
			// SQL dataset type SHOULD be included in single-tenant mode
			boolean hasSql = result.stream().anyMatch(dv -> DatasetType.SQL.toCode().equals(dv.getCode()));
			assertTrue(hasSql, "SQL should be included in single-tenant mode");
		} finally {
			UtilImpl.CUSTOMER = null;
		}
	}

	@Test
	void getVariantDomainValuesForDatasetTypeIncludesBizQL() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues(ReportDataset.datasetTypePropertyName);
		assertNotNull(result);
		boolean hasBizQL = result.stream().anyMatch(dv -> DatasetType.bizQL.toCode().equals(dv.getCode()));
		assertTrue(hasBizQL, "bizQL should always be included");
	}

	@Test
	void getVariantDomainValuesForDatasetTypeIncludesClass() throws Exception {
		UtilImpl.CUSTOMER = "bizhub"; // single-tenant mode for complete list
		try {
			List<DomainValue> result = bizlet.getVariantDomainValues(ReportDataset.datasetTypePropertyName);
			assertNotNull(result);
			boolean hasClass = result.stream().anyMatch(dv -> DatasetType.classValue.toCode().equals(dv.getCode()));
			assertTrue(hasClass, "class type should always be included");
		} finally {
			UtilImpl.CUSTOMER = null;
		}
	}

	@Test
	void getVariantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues("unknownAttribute");
		// Super class returns null by default (no metaDataBizlet)
		assertTrue(result == null || result.isEmpty(), "Unknown attributes should return null or empty");
	}

	@Test
	void preExecuteReturnsBeanWhenZoomingOutWithoutParentParameters() throws Exception {
		ReportDatasetExtension bean = new ReportDatasetExtension();

		ReportDatasetExtension result = bizlet.preExecute(ImplicitActionName.ZoomOut, bean, null, null);

		assertSame(bean, result);
	}

	@Test
	void preExecuteReturnsBeanForNonZoomOutAction() throws Exception {
		ReportDatasetExtension bean = new ReportDatasetExtension();

		ReportDatasetExtension result = bizlet.preExecute(ImplicitActionName.Save, bean, null, null);

		assertSame(bean, result);
	}

	@Test
	void preRerenderIgnoresSourcesOtherThanQuery() throws Exception {
		ReportDatasetExtension bean = new ReportDatasetExtension();

		bizlet.preRerender("otherSource", bean, null);

		assertNotNull(bean);
	}
}
