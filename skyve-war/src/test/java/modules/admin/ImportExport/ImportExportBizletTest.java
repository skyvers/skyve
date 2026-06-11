package modules.admin.ImportExport;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import modules.admin.domain.ImportExport;
import util.AbstractH2Test;

class ImportExportBizletTest extends AbstractH2Test {

	private ImportExportBizlet bizlet;
	private ImportExportExtension bean;

	@BeforeEach
	void initBizlet() {
		bizlet = new ImportExportBizlet();
		bean = ImportExport.newInstance();
	}

	@Test
	void completeLoadTypeAttributeReturnsTwoOptions() throws Exception {
		List<String> result = bizlet.complete(ImportExport.loadTypePropertyName, "", bean);
		assertNotNull(result);
		assertTrue(result.size() >= 2);
		assertTrue(result.contains(ImportExportUtil.CREATE_RELATED_RECORDS_IF_THEY_DON_T_EXIST));
		assertTrue(result.contains(ImportExportUtil.CREATE_EVERYTHING_EVEN_IF_THERE_MIGHT_BE_DUPLICATES));
	}

	@Test
	void completeUnknownAttributeReturnsEmpty() throws Exception {
		List<String> result = bizlet.complete("unknownAttr", "", bean);
		assertTrue(result == null || result.isEmpty());
	}

	@Test
	void getConstantDomainValuesForModuleNameReturnsModules() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(ImportExport.moduleNamePropertyName);
		assertNotNull(result);
		assertTrue(result.size() > 0);
	}

	@Test
	void getConstantDomainValuesForOtherAttributeCallsSuper() throws Exception {
		// super returns null for unknown attributes
		Assertions.assertNull(bizlet.getConstantDomainValues("unknownAttr"));
	}

	@Test
	void getDynamicDomainValuesForDocumentNameWithNullModuleReturnsEmpty() throws Exception {
		bean.setModuleName(null);
		List<DomainValue> result = bizlet.getDynamicDomainValues(ImportExport.documentNamePropertyName, bean);
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void getDynamicDomainValuesForDocumentNameWithModuleReturnsDocuments() throws Exception {
		bean.setModuleName("admin");
		List<DomainValue> result = bizlet.getDynamicDomainValues(ImportExport.documentNamePropertyName, bean);
		assertNotNull(result);
		assertTrue(result.size() > 0);
	}

	@Test
	void getDynamicDomainValuesForOtherAttributeCallsSuper() throws Exception {
		// super returns null for unknown attributes
		Assertions.assertNull(bizlet.getDynamicDomainValues("unknownAttr", bean));
	}
}
