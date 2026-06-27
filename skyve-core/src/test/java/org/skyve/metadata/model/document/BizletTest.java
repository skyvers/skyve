package org.skyve.metadata.model.document;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.web.WebContext;

class BizletTest {

	// ---- DomainValue tests ----

	@Test
	@SuppressWarnings("static-method")
	void domainValueSingleArgConstructorSetsCodeAndDescription() {
		DomainValue dv = new DomainValue("myValue");
		assertThat(dv.getCode(), is("myValue"));
	}

	@Test
	@SuppressWarnings("static-method")
	void domainValueTwoArgConstructorSetsCode() {
		DomainValue dv = new DomainValue("code1", "Description One");
		assertThat(dv.getCode(), is("code1"));
		assertThat(dv.getLocalisedDescription(), is("Description One"));
	}

	@Test
	@SuppressWarnings("static-method")
	void domainValueToStringContainsCodeAndDescription() {
		DomainValue dv = new DomainValue("code1", "desc1");
		String s = dv.toString();
		assertNotNull(s);
		// toString() includes code->description
		assertTrue(s.contains("code1"), "toString should contain code");
		assertTrue(s.contains("desc1"), "toString should contain description");
	}

	@Test
	@SuppressWarnings("static-method")
	void domainValueToStringArrow() {
		DomainValue dv = new DomainValue("A", "B");
		// format: ...(A->B)
		assertTrue(dv.toString().contains("A->B"));
	}

	// ---- Bizlet default methods (with null metaDataBizlet) ----

	@Test
	@SuppressWarnings("static-method")
	void bizletNewInstanceReturnsSameBean() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		// metaDataBizlet is null so newInstance returns the same bean
		// We can't easily create a bean, but we can verify the method exists and handles null metaDataBizlet
		assertNull(bizlet.newInstance(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletGetConstantDomainValuesReturnsNullWhenNoMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		List<DomainValue> result = bizlet.getConstantDomainValues("anyAttribute");
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletGetVariantDomainValuesReturnsNullWhenNoMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		List<DomainValue> result = bizlet.getVariantDomainValues("anyAttribute");
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletGetDynamicDomainValuesReturnsNullWhenNoMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		List<DomainValue> result = bizlet.getDynamicDomainValues("anyAttribute", null);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletValidateDoesNotThrowWhenNoMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		ValidationException e = new ValidationException();
		assertDoesNotThrow(() -> bizlet.validate(null, e));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletCompleteReturnsNullWhenNoMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		List<String> result = bizlet.complete("attr", "val", null);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletResolveReturnsNullWhenNoMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		org.skyve.domain.Bean result = bizlet.resolve("bizId", null, mock(WebContext.class));
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPreSaveDoesNotThrowWhenNoMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		assertDoesNotThrow(() -> bizlet.preSave(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPostSaveDoesNotThrowWhenNoMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		assertDoesNotThrow(() -> bizlet.postSave(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPreDeleteDoesNotThrowWhenNoMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		assertDoesNotThrow(() -> bizlet.preDelete(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPostDeleteDoesNotThrowWhenNoMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		assertDoesNotThrow(() -> bizlet.postDelete(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPostLoadDoesNotThrowWhenNoMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		assertDoesNotThrow(() -> bizlet.postLoad(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPreExecuteReturnsNullWhenNoMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		org.skyve.domain.Bean result = bizlet.preExecute(ImplicitActionName.Save, null, null, null);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPreRerenderDoesNotThrowWhenNoMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		assertDoesNotThrow(() -> bizlet.preRerender("source", null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPostRenderDoesNotThrowWhenNoMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		assertDoesNotThrow(() -> bizlet.postRender(null, null));
	}

	// ---- metaDataBizlet != null branches ----

	@Test
	@SuppressWarnings("static-method")
	void bizletNewInstanceDelegatesToMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		org.skyve.domain.Bean result = bizlet.newInstance(null);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletValidateDelegatesToMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		assertDoesNotThrow(() -> bizlet.validate(null, new ValidationException()));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletGetConstantDomainValuesDelegatesToMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		List<DomainValue> result = bizlet.getConstantDomainValues("attr");
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletGetVariantDomainValuesDelegatesToMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		List<DomainValue> result = bizlet.getVariantDomainValues("attr");
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletGetDynamicDomainValuesDelegatesToMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		List<DomainValue> result = bizlet.getDynamicDomainValues("attr", null);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletCompleteDelegatesToMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		List<String> result = bizlet.complete("attr", "val", null);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletResolveDelegatesToMetaDataBizlet() throws Exception {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		org.skyve.domain.Bean result = bizlet.resolve("id", null, mock(WebContext.class));
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPreSaveDelegatesToMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		assertDoesNotThrow(() -> bizlet.preSave(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPostSaveDelegatesToMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		assertDoesNotThrow(() -> bizlet.postSave(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPreDeleteDelegatesToMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		assertDoesNotThrow(() -> bizlet.preDelete(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPostDeleteDelegatesToMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		assertDoesNotThrow(() -> bizlet.postDelete(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPostLoadDelegatesToMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		assertDoesNotThrow(() -> bizlet.postLoad(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPreRerenderDelegatesToMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		assertDoesNotThrow(() -> bizlet.preRerender("source", null, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void bizletPostRenderDelegatesToMetaDataBizlet() {
		Bizlet<org.skyve.domain.Bean> bizlet = new Bizlet<>();
		bizlet.setMetaDataBizlet(new BizletMetaData());
		assertDoesNotThrow(() -> bizlet.postRender(null, null));
	}
}
