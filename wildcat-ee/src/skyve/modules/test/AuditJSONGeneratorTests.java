package modules.test;

import modules.AuditJSONGenerator;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesRequiredPersistent;
import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtension;

import org.junit.Test;
import org.skyve.util.Util;

public class AuditJSONGeneratorTests extends AbstractH2Test {
	@Test
	public void testAllAttributesPersistent() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(aapd, test, c);
		System.out.println("Audit AAP = " + ajg.toJSON());
	}

	@Test
	public void testAllAttributesRequiredPersistent() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(aarpd, test, c);
		System.out.println("Audit AARP  = " + ajg.toJSON());
	}

	@Test
	public void testMappedBase() throws Exception {
		MappedBase test = Util.constructRandomInstance(u, m, mbd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(mbd, test, c);
		System.out.println("Audit MB = " + ajg.toJSON());
	}

	@Test
	public void testMappedExtension() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(med, test, c);
		System.out.println("Audit ME = " + ajg.toJSON());
	}
}
