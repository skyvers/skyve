package modules.test;

import modules.test.domain.MappedExtensionSingleStrategy;

import org.junit.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.BeanValidator;
import org.skyve.util.Util;

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
}
