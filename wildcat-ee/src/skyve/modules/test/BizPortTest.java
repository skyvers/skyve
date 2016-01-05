package modules.test;

import modules.ModulesUtil;
import modules.test.domain.MappedExtension;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.bizport.BizPortException;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.util.Util;

public class BizPortTest extends AbstractH2Test {
	@Test
	public void testStandardExport() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 3);
		test = p.save(test);
		BizPortWorkbook workbook = ModulesUtil.standardBeanBizExport(m.getName(), med.getName(), test);
		BizPortException problems = new BizPortException();
		ModulesUtil.standardBeanBizImport(workbook, problems);
		Assert.assertFalse(problems.hasErrors());
	}
}
