package modules.test;

import org.junit.Test;
import org.skyve.util.Util;

import modules.admin.AuditJSONGenerator;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesRequiredPersistent;
import modules.test.domain.InverseManyToManyPersistent;
import modules.test.domain.InverseOneToManyPersistent;
import modules.test.domain.InverseOneToOnePersistent;
import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

public class AuditJSONGeneratorTests extends AbstractSkyveTest {
	@Test
	public void testAllAttributesPersistent() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(aapd, test, c);
		ajg.toJSON();
	}

	@Test
	public void testAllAttributesRequiredPersistent() throws Exception {
		AllAttributesRequiredPersistent test = Util.constructRandomInstance(u, m, aarpd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(aarpd, test, c);
		ajg.toJSON();
	}

	@Test
	public void testInverseOneToOnePersistent() throws Exception {
		InverseOneToOnePersistent test = Util.constructRandomInstance(u, m, io2opd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(io2opd, test, c);
		ajg.toJSON();
	}

	@Test
	public void testInverseOneToManyPersistent() throws Exception {
		InverseOneToManyPersistent test = Util.constructRandomInstance(u, m, io2mpd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(io2mpd, test, c);
		ajg.toJSON();
	}

	@Test
	public void testInverseManyToManyPersistent() throws Exception {
		InverseManyToManyPersistent test = Util.constructRandomInstance(u, m, im2mpd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(im2mpd, test, c);
		ajg.toJSON();
	}

	@Test
	public void testMappedBase() throws Exception {
		MappedBase test = Util.constructRandomInstance(u, m, mbd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(mbd, test, c);
		ajg.toJSON();
	}

	@Test
	public void testMappedExtensionSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(messd, test, c);
		ajg.toJSON();
	}

	@Test
	public void testMappedExtensionJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(mejsd, test, c);
		ajg.toJSON();
	}

	@Test
	public void testMappedSubclassedSingleStrategy() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(msssd, test, c);
		ajg.toJSON();
	}

	@Test
	public void testMappedSubclassedJoinedStrategy() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(msjsd, test, c);
		ajg.toJSON();
	}
}
