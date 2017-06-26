package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.BeanValidator;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesRequiredPersistent;
import modules.test.domain.MappedExtensionSingleStrategy;
import util.AbstractH2Test;

public class BeanValidatorTests extends AbstractH2Test {
	@Test
	public void testValidateBeanAgainstDocument() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		BeanValidator.validateBeanAgainstDocument(messd, test);
	}

	@Test(expected = ValidationException.class)
	public void testValidateBeanAgainstBaseDocument() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test.setColour(null);
		BeanValidator.validateBeanAgainstDocument(messd, test);
	}

	@Test
	public void testValidateBeanAgainstDocumentSuccess() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		BeanValidator.validateBeanAgainstDocument(aarpd, test);
	}

	@Test(expected = ValidationException.class)
	public void testValidateBeanAgainstDocumentFail() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		test.setColour(null);
		BeanValidator.validateBeanAgainstDocument(aarpd, test);
	}

	@Test
	public void testValidateBeanAgainstDocumentNameSuccess() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		BeanValidator.validateBeanAgainstDocument(test);
	}

	@Test(expected = ValidationException.class)
	public void testValidateBeanAgainstDocumentNamecFail() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		test.setColour(null);
		BeanValidator.validateBeanAgainstDocument(test);
	}

	@Test
	public void testValidateBeanPropertyAgainstAttributeSuccess() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(CORE.getUser().getCustomer(),
				aarpd.getAttribute(AllAttributesRequiredPersistent.colourPropertyName),
				test,
				ve);
		Assert.assertEquals("No validation error should occur", 0, ve.getMessages().size());
	}

	@Test
	public void testValidateBeanPropertyAgainstAttributeFail() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		test.setColour(null);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(CORE.getUser().getCustomer(),
				aarpd.getAttribute(AllAttributesRequiredPersistent.colourPropertyName),
				test,
				ve);
		Assert.assertEquals("No validation error should occur", 1, ve.getMessages().size());
	}

	@Test
	public void testValidateBeanPropertyAgainstAttributeWithoutCustomerSuccess() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(aarpd.getAttribute(AllAttributesRequiredPersistent.colourPropertyName),
				test,
				ve);
		Assert.assertEquals("No validation error should occur", 0, ve.getMessages().size());
	}

	@Test
	public void testValidateBeanPropertyAgainstAttributeWithoutCustomerFail() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		test.setColour(null);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(aarpd.getAttribute(AllAttributesRequiredPersistent.colourPropertyName),
				test,
				ve);
		Assert.assertEquals("No validation error should occur", 1, ve.getMessages().size());
	}

	@Test
	public void testValidateBeanPropertyAgainstAttributeNameSuccess() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(AllAttributesRequiredPersistent.colourPropertyName,
				test,
				ve);
		Assert.assertEquals("No validation error should occur", 0, ve.getMessages().size());
	}

	@Test
	public void testValidateBeanPropertyAgainstAttributeNameFail() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 2);
		test.setColour(null);
		ValidationException ve = new ValidationException();
		BeanValidator.validateBeanPropertyAgainstAttribute(AllAttributesRequiredPersistent.colourPropertyName,
				test,
				ve);
		Assert.assertEquals("No validation error should occur", 1, ve.getMessages().size());
	}
}
