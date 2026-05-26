package modules.test.AllAttributesDynamicPersistent;

import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ImplicitActionName;

@SuppressWarnings("static-method")
public class TestBizletTest {

	private static final TestBizlet bizlet = new TestBizlet();

	private static DynamicPersistentBean newBean() {
		return new DynamicPersistentBean("test", "AllAttributesDynamicPersistent", Map.of());
	}

	@Test
	void newInstanceReturnsBean() throws Exception {
		DynamicPersistentBean bean = newBean();
		bizlet.newInstance(bean);
	}

	@Test
	void validateDoesNotThrow() throws Exception {
		DynamicPersistentBean bean = newBean();
		bizlet.validate(bean, new ValidationException());
	}

	@Test
	void getConstantDomainValuesReturnsNull() throws Exception {
		assertNull(bizlet.getConstantDomainValues("anyAttr"));
	}

	@Test
	void getVariantDomainValuesReturnsNull() throws Exception {
		assertNull(bizlet.getVariantDomainValues("anyAttr"));
	}

	@Test
	void getDynamicDomainValuesReturnsNull() throws Exception {
		DynamicPersistentBean bean = newBean();
		assertNull(bizlet.getDynamicDomainValues("anyAttr", bean));
	}

	@Test
	void completeReturnsNull() throws Exception {
		DynamicPersistentBean bean = newBean();
		assertNull(bizlet.complete("anyAttr", "val", bean));
	}

	@Test
	void resolveReturnsNull() throws Exception {
		assertNull(bizlet.resolve("some-id", null, null));
	}

	@Test
	void preSaveDoesNotThrow() throws Exception {
		DynamicPersistentBean bean = newBean();
		bizlet.preSave(bean);
	}

	@Test
	void postSaveDoesNotThrow() throws Exception {
		DynamicPersistentBean bean = newBean();
		bizlet.postSave(bean);
	}

	@Test
	void preDeleteDoesNotThrow() throws Exception {
		DynamicPersistentBean bean = newBean();
		bizlet.preDelete(bean);
	}

	@Test
	void postLoadDoesNotThrow() throws Exception {
		DynamicPersistentBean bean = newBean();
		bizlet.postLoad(bean);
	}

	@Test
	void preExecuteReturnsBean() throws Exception {
		DynamicPersistentBean bean = newBean();
		bizlet.preExecute(ImplicitActionName.Edit, bean, null, null);
	}

	@Test
	void preRerenderDoesNotThrow() throws Exception {
		DynamicPersistentBean bean = newBean();
		bizlet.preRerender("source", bean, null);
	}
}
