package modules.test;

import modules.test.domain.MappedExtension;

import org.junit.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.BeanValidator;
import org.skyve.util.Util;

public class BeanValidatorTests extends AbstractH2Test {
	@Test
	public void testValidateBeanAgainstDocument() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 2);
		BeanValidator.validateBeanAgainstDocument(med, test);
	}
	
	@Test(expected = ValidationException.class)
	public void testValidateBeanAgainstBaseDocument() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 2);
		test.setColour(null);
		BeanValidator.validateBeanAgainstDocument(med, test);
	}
}
