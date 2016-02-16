package modules.test;

import modules.AuditJSONGenerator;
import modules.test.domain.AllAttributesInverseOneToOnePersistent;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesRequiredPersistent;
import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

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
	public void testAllAttributesInverseOneToOnePersistent() throws Exception {
		AllAttributesInverseOneToOnePersistent test = Util.constructRandomInstance(u, m, aai121pd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(aai121pd, test, c);
		System.out.println("Audit AAI121RP  = " + ajg.toJSON());
	}

	@Test
	public void testMappedBase() throws Exception {
		MappedBase test = Util.constructRandomInstance(u, m, mbd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(mbd, test, c);
		System.out.println("Audit MB = " + ajg.toJSON());
	}

	@Test
	public void testMappedExtensionSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(messd, test, c);
		System.out.println("Audit MESS = " + ajg.toJSON());
	}

	@Test
	public void testMappedExtensionJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(mejsd, test, c);
		System.out.println("Audit MEJS = " + ajg.toJSON());
	}

	@Test
	public void testMappedSubclassedSingleStrategy() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(msssd, test, c);
		System.out.println("Audit MSSS = " + ajg.toJSON());
	}

	@Test
	public void testMappedSubclassedJoinedStrategy() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(msjsd, test, c);
		System.out.println("Audit MSJS = " + ajg.toJSON());
	}
}
