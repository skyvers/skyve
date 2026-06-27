package modules.admin.Audit;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ImplicitActionName;

import modules.admin.domain.Audit;
@SuppressWarnings("static-method")
class AuditBizletTest {

	@Test
	void preExecuteNewReturnsBean() {
		AuditBizlet bizlet = new AuditBizlet();
		Audit bean = new Audit();
		Audit result = bizlet.preExecute(ImplicitActionName.New, bean, null, null);
		assertSame(bean, result);
	}

	@Test
	void getDynamicDomainValuesUnknownAttributeReturnsNull() {
		AuditBizlet bizlet = new AuditBizlet();
		Audit bean = new Audit();
		assertNull(bizlet.getDynamicDomainValues("unknownAttribute", bean));
	}

	@Test
	void getDynamicDomainValuesComparisonVersionWithNullSourceVersionReturnsNull() {
		AuditBizlet bizlet = new AuditBizlet();
		Audit bean = new Audit();
		// sourceVersion is null by default, so this branch returns null
		assertNull(bizlet.getDynamicDomainValues(Audit.comparisonVersionPropertyName, bean));
	}
}
