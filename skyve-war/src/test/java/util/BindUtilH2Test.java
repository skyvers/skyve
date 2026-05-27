package util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.CORE;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

import modules.test.domain.AllAttributesPersistent;

/**
 * H2-backed tests for {@link BindUtil} methods that require a real
 * persistence context with Customer, Module, and Document metadata.
 *
 * Covers: populateProperty, set (nested bindings), isMutable,
 * getMetaDataForBinding, getPropertyType, validateBinding, copy.
 */
@SuppressWarnings({ "static-method", "java:S1130" })
class BindUtilH2Test extends AbstractH2Test {

	private AllAttributesPersistent bean;
	private AllAttributesPersistent assocBean;
	private Customer customer;
	private User user;
	private Module module;
	private Document document;

	@BeforeEach
	void setup() throws Exception {
		Persistence p = CORE.getPersistence();
		customer = CORE.getCustomer();
		user = CORE.getUser();
		module = customer.getModule(AllAttributesPersistent.MODULE_NAME);
		document = module.getDocument(customer, AllAttributesPersistent.DOCUMENT_NAME);

		bean = AllAttributesPersistent.newInstance();
		bean.setText("root");
		bean = p.save(bean);

		assocBean = AllAttributesPersistent.newInstance();
		assocBean.setText("assoc");
		assocBean = p.save(assocBean);

		bean.setAggregatedAssociation(assocBean);
		bean = p.save(bean);
	}

	// ---- populateProperty ----

	@Test
	void populatePropertySetsSimpleProperty() throws Exception {
		AllAttributesPersistent bean2 = AllAttributesPersistent.newInstance();
		BindUtil.populateProperty(user, bean2, AllAttributesPersistent.textPropertyName, "hello", false);
		assertEquals("hello", bean2.getText());
	}

	@Test
	void populatePropertySetsIntegerProperty() throws Exception {
		AllAttributesPersistent bean2 = AllAttributesPersistent.newInstance();
		BindUtil.populateProperty(user, bean2, AllAttributesPersistent.normalIntegerPropertyName, "42", false);
		assertEquals(Integer.valueOf(42), bean2.getNormalInteger());
	}

	@Test
	void populatePropertyWithNullValueClearsProperty() throws Exception {
		AllAttributesPersistent bean2 = AllAttributesPersistent.newInstance();
		bean2.setText("initial");
		BindUtil.populateProperty(user, bean2, AllAttributesPersistent.textPropertyName, null, false);
		assertNull(bean2.getText());
	}

	@Test
	void populatePropertyWithEmptyStringClearsProperty() throws Exception {
		AllAttributesPersistent bean2 = AllAttributesPersistent.newInstance();
		bean2.setText("initial");
		BindUtil.populateProperty(user, bean2, AllAttributesPersistent.textPropertyName, "", false);
		assertNull(bean2.getText());
	}

	// ---- isMutable ----

	@Test
	void isMutableReturnsTrueForRegularProperty() {
		assertTrue(BindUtil.isMutable(bean, AllAttributesPersistent.textPropertyName));
	}

	@Test
	void isMutableReturnsFalseForBizKey() {
		assertFalse(BindUtil.isMutable(bean, "bizKey"));
	}

	@Test
	void isMutableReturnsFalseForBizModule() {
		assertFalse(BindUtil.isMutable(bean, "bizModule"));
	}

	@Test
	void isMutableReturnsFalseForBizDocument() {
		assertFalse(BindUtil.isMutable(bean, "bizDocument"));
	}

	@Test
	void isMutableReturnsFalseForCreated() {
		assertFalse(BindUtil.isMutable(bean, "created"));
	}

	@Test
	void isMutableReturnsFalseForPersisted() {
		assertFalse(BindUtil.isMutable(bean, "persisted"));
	}

	@Test
	void isMutableReturnsFalseForChanged() {
		assertFalse(BindUtil.isMutable(bean, "changed"));
	}

	@Test
	void isMutableReturnsFalseForNotCreated() {
		assertFalse(BindUtil.isMutable(bean, "notCreated"));
	}

	@Test
	void isMutableReturnsFalseForNotPersisted() {
		assertFalse(BindUtil.isMutable(bean, "notPersisted"));
	}

	@Test
	void isMutableReturnsFalseForNotChanged() {
		assertFalse(BindUtil.isMutable(bean, "notChanged"));
	}

	// ---- getMetaDataForBinding ----

	@Test
	void getMetaDataForBindingReturnsAttributeForSimpleField() throws Exception {
		var target = BindUtil.getMetaDataForBinding(customer, module, document,
				AllAttributesPersistent.textPropertyName);
		assertNotNull(target);
		Attribute attr = target.getAttribute();
		assertNotNull(attr);
		assertEquals("text", attr.getName());
	}

	@Test
	void getMetaDataForBindingReturnsDotNotationAttribute() throws Exception {
		var target = BindUtil.getMetaDataForBinding(customer, module, document,
				AllAttributesPersistent.aggregatedAssociationPropertyName + ".text");
		assertNotNull(target);
		Attribute attr = target.getAttribute();
		assertNotNull(attr);
		assertEquals("text", attr.getName());
	}

	// ---- getPropertyType ----

	@Test
	void getPropertyTypeReturnsStringForTextField() {
		Class<?> type = BindUtil.getPropertyType(bean, AllAttributesPersistent.textPropertyName);
		assertEquals(String.class, type);
	}

	@Test
	void getPropertyTypeReturnsIntegerForNormalIntegerField() {
		Class<?> type = BindUtil.getPropertyType(bean, AllAttributesPersistent.normalIntegerPropertyName);
		assertEquals(Integer.class, type);
	}

	// ---- validateBinding ----

	@Test
	void validateBindingReturnsTargetForSimpleField() throws Exception {
		var target = BindUtil.validateBinding(customer, module, document,
				AllAttributesPersistent.textPropertyName);
		assertNotNull(target);
		assertEquals("text", target.getAttribute().getName());
	}

	@Test
	void validateBindingThrowsForInvalidField() {
		Assertions.assertThrows(Exception.class,
				() -> BindUtil.validateBinding(customer, module, document, "notAField"));
	}

	// ---- set and get for nested binding ----

	@Test
	void getNestedBindingReturnsAssocText() {
		Object val = BindUtil.get(bean,
				AllAttributesPersistent.aggregatedAssociationPropertyName + ".text");
		assertEquals("assoc", val);
	}

	@Test
	void setSimplePropertyViaBindUtil() {
		BindUtil.set(bean, AllAttributesPersistent.textPropertyName, "updated");
		assertEquals("updated", bean.getText());
	}

	@Test
	void setNullPropertyViaBindUtil() {
		bean.setText("initial");
		BindUtil.set(bean, AllAttributesPersistent.textPropertyName, null);
		assertNull(bean.getText());
	}

	// ---- copy ----

	@Test
	void copyPropertiesFromOneBeanToAnother() throws Exception {
		AllAttributesPersistent source = AllAttributesPersistent.newInstance();
		source.setText("copyme");
		source.setNormalInteger(Integer.valueOf(99));

		AllAttributesPersistent dest = AllAttributesPersistent.newInstance();
		BindUtil.copy(source, dest);

		assertEquals("copyme", dest.getText());
		assertEquals(Integer.valueOf(99), dest.getNormalInteger());
	}

	// ---- setRelationInverse ----

	@Test
	void setRelationInverseDoesNotThrowForSimpleAssociation() throws Exception {
		// setRelationInverse is a no-op for non-inverse associations
		// This mainly tests it doesn't throw
		AllAttributesPersistent child = AllAttributesPersistent.newInstance();
		child.setText("child");
		child = CORE.getPersistence().save(child);

		// Just call set on an association field to exercise the set path including setRelationInverse
		BindUtil.set(bean, AllAttributesPersistent.aggregatedAssociationPropertyName, child);
		assertEquals(child.getBizId(), bean.getAggregatedAssociation().getBizId());
	}
}
