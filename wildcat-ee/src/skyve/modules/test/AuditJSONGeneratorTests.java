package modules.test;

import org.junit.Before;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

import modules.AuditJSONGenerator;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesRequiredPersistent;
import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtension;

public class AuditJSONGeneratorTests extends AbstractH2Test {
	private Persistence p;
	private User u;
	private Customer c;
	private Module m;
	private Document aapd;
	private Document aarpd;
	private Document mb;
	private Document me;


	@Before
	public void before() throws Exception {
		p = CORE.getPersistence();
		u = p.getUser();
		c = u.getCustomer();
		m = c.getModule(AllAttributesPersistent.MODULE_NAME);
		aapd = m.getDocument(c, AllAttributesPersistent.DOCUMENT_NAME);
		aarpd = m.getDocument(c, AllAttributesRequiredPersistent.DOCUMENT_NAME);
		mb = m.getDocument(c, MappedBase.DOCUMENT_NAME);
		me = m.getDocument(c, MappedExtension.DOCUMENT_NAME);
	}

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
		MappedBase test = Util.constructRandomInstance(u, m, mb, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(mb, test, c);
		System.out.println("Audit MB = " + ajg.toJSON());
	}

	@Test
	public void testMappedExtension() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, me, 5);
		AuditJSONGenerator ajg = new AuditJSONGenerator(c);
		ajg.visit(me, test, c);
		System.out.println("Audit ME = " + ajg.toJSON());
	}
}
