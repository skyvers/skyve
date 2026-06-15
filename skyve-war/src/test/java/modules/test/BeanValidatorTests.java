package modules.test;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.BeanValidator;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesRequiredPersistent;
import modules.test.domain.MappedExtensionSingleStrategy;

@SuppressWarnings("java:S5778")
class BeanValidatorTests extends AbstractSkyveTest {

	@Test
	void testValidateBeanAgainstDocument() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		BeanValidator.validateBeanAgainstDocument(messd, test);
	}

	@Test
	void testValidateBeanAgainstBaseDocument() {
		ValidationException ve = Assertions.assertThrows(ValidationException.class, () -> {
			MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
			test.setColour(null);
			BeanValidator.validateBeanAgainstDocument(messd, test);
		});

		assertTrue(ve.getMessages().size() > 0);
	}

	@Test
	void testValidateBeanAgainstDocumentSuccess() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		BeanValidator.validateBeanAgainstDocument(aarpd, test);
	}

	@Test
	void testValidateBeanAgainstDocumentFail() {
		ValidationException ve = Assertions.assertThrows(ValidationException.class, () -> {
			AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
			test.setColour(null);
			BeanValidator.validateBeanAgainstDocument(aarpd, test);
		});

		assertTrue(ve.getMessages().size() > 0);
	}

	@Test
	void testValidateBeanAgainstDocumentNameSuccess() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		BeanValidator.validateBeanAgainstDocument(test);
	}

	@Test
	void testValidateBeanAgainstDocumentNamecFail() {
		ValidationException ve = Assertions.assertThrows(ValidationException.class, () -> {
			AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
			test.setColour(null);
			BeanValidator.validateBeanAgainstDocument(test);
		});

		assertTrue(ve.getMessages().size() > 0);
	}

	@Test
	void testValidateBeanPropertyAgainstAttributeSuccess() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(CORE.getUser(),
				aarpd.getAttribute(AllAttributesRequiredPersistent.colourPropertyName),
				test,
				ve);
		Assertions.assertEquals(0, ve.getMessages().size(), "No validation error should occur");
	}

	@Test
	void testValidateBeanPropertyAgainstAttributeFail() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		test.setColour(null);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(CORE.getUser(),
				aarpd.getAttribute(AllAttributesRequiredPersistent.colourPropertyName),
				test,
				ve);
		Assertions.assertEquals(1, ve.getMessages().size(), "No validation error should occur");
	}

	@Test
	void testValidateBeanPropertyAgainstAttributeWithoutCustomerSuccess() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(aarpd.getAttribute(AllAttributesRequiredPersistent.colourPropertyName),
				test,
				ve);
		Assertions.assertEquals(0, ve.getMessages().size(), "No validation error should occur");
	}

	@Test
	void testValidateBeanPropertyAgainstAttributeWithoutCustomerFail() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		test.setColour(null);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(aarpd.getAttribute(AllAttributesRequiredPersistent.colourPropertyName),
				test,
				ve);
		Assertions.assertEquals(1, ve.getMessages().size(), "No validation error should occur");
	}

	@Test
	void testValidateBeanPropertyAgainstAttributeNameSuccess() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(AllAttributesRequiredPersistent.colourPropertyName,
				test,
				ve);
		Assertions.assertEquals(0, ve.getMessages().size(), "No validation error should occur");
	}

	@Test
	void testValidateBeanPropertyAgainstAttributeNameFail() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		test.setColour(null);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(AllAttributesRequiredPersistent.colourPropertyName,
				test,
				ve);
		Assertions.assertEquals(1, ve.getMessages().size(), "No validation error should occur");
	}
}
