package modules.test;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.bizport.BizPortException;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

import modules.ModulesUtil;
import modules.test.domain.MappedExtension;

public class BizPortTest extends AbstractH2Test {
	private Persistence p;
	private User u;
	private Customer c;
	private Module m;
	private Document d;

	@Before
	public void before() throws Exception {
		p = CORE.getPersistence();
		u = p.getUser();
		c = u.getCustomer();
		m = c.getModule(MappedExtension.MODULE_NAME);
		d = m.getDocument(c, MappedExtension.DOCUMENT_NAME);
	}

	@Test
	public void testStandardExport() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, d, 3);
		test = p.save(test);
		BizPortWorkbook workbook = ModulesUtil.standardBeanBizExport(m.getName(), d.getName(), test);
		BizPortException problems = new BizPortException();
		ModulesUtil.standardBeanBizImport(workbook, problems);
		Assert.assertFalse(problems.hasErrors());
	}
}
