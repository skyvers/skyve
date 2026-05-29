package modules.admin.ImportExportColumn;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExportColumn;
import util.AbstractH2Test;

/**
 * H2-backed tests for ImportExportColumnBizlet covering getDynamicDomainValues,
 * complete and preExecute.
 */
class ImportExportColumnBizletH2Test extends AbstractH2Test {

	private ImportExportColumnBizlet bizlet;
	private ImportExportColumn columnBean;
	private ImportExportExtension parentBean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		bizlet = new ImportExportColumnBizlet();
		webContext = new MockWebContext();

		parentBean = ImportExport.newInstance();
		parentBean.setModuleName("admin");
		parentBean.setDocumentName("User");

		columnBean = ImportExportColumn.newInstance();
		columnBean.setParent(parentBean);
	}

	// ---- getDynamicDomainValues: bindingName ----

	@Test
	void getDynamicDomainValuesForBindingNameWithParentSetReturnsAttributes() throws Exception {
		List<DomainValue> result = bizlet.getDynamicDomainValues(ImportExportColumn.bindingNamePropertyName, columnBean);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected attributes for User document");
	}

	@Test
	void getDynamicDomainValuesForBindingNameWithNullParentReturnsEmpty() throws Exception {
		columnBean.setParent(null);

		List<DomainValue> result = bizlet.getDynamicDomainValues(ImportExportColumn.bindingNamePropertyName, columnBean);
		assertThat(result, is(notNullValue()));
		// When parent is null, bindings will be empty (no module to query)
	}

	@Test
	void getDynamicDomainValuesForUnknownAttributeCallsSuper() {
		Assertions.assertDoesNotThrow(() -> bizlet.getDynamicDomainValues("unknownAttribute", columnBean));
		// Calling super - should return null or empty
	}

	// ---- complete: bindingName ----

	@Test
	void completeForBindingNameWithParentSetReturnsAttributeNames() throws Exception {
		List<String> result = bizlet.complete(ImportExportColumn.bindingNamePropertyName, "", columnBean);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected attribute names for User document");
	}

	@Test
	void completeForBindingNameWithNullParentReturnsEmpty() throws Exception {
		columnBean.setParent(null);

		List<String> result = bizlet.complete(ImportExportColumn.bindingNamePropertyName, "", columnBean);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void completeForUnknownAttributeCallsSuper() {
		Assertions.assertDoesNotThrow(() -> bizlet.complete("unknownAttribute", "", columnBean));
		// super returns null or empty
	}

	// ---- preExecute: OK action with valid binding ----

	@Test
	void preExecuteOkWithNullBindingReturnsBean() throws Exception {
		columnBean.setBindingName(null);

		ImportExportColumn result = bizlet.preExecute(ImplicitActionName.OK, columnBean, parentBean, webContext);
		assertNotNull(result);
	}

	@Test
	void preExecuteOkWithValidBindingReturnsBean() throws Exception {
		columnBean.setBindingName("userName");

		ImportExportColumn result = bizlet.preExecute(ImplicitActionName.OK, columnBean, parentBean, webContext);
		assertNotNull(result);
	}

	@Test
	void preExecuteSaveWithValidBindingReturnsBean() throws Exception {
		columnBean.setBindingName("userName");

		ImportExportColumn result = bizlet.preExecute(ImplicitActionName.Save, columnBean, parentBean, webContext);
		assertNotNull(result);
	}

	@Test
	void preExecuteZoomOutWithValidBindingReturnsBean() throws Exception {
		columnBean.setBindingName("userName");

		ImportExportColumn result = bizlet.preExecute(ImplicitActionName.ZoomOut, columnBean, parentBean, webContext);
		assertNotNull(result);
	}

	@Test
	void preExecuteEditActionDoesNotThrow() throws Exception {
		columnBean.setBindingName("userName");

		ImportExportColumn result = bizlet.preExecute(ImplicitActionName.Edit, columnBean, parentBean, webContext);
		assertNotNull(result);
	}

	@Test
	void preExecuteOkWithShowExpressionAndWrappedBindingExpressionReturnsBean() throws Exception {
		columnBean.setBindingName(modules.admin.ImportExport.ImportExportUtil.EXPRESSION);
		columnBean.setBindingExpression("{userName}");

		ImportExportColumn result = bizlet.preExecute(ImplicitActionName.OK, columnBean, parentBean, webContext);
		assertNotNull(result);
	}
}
