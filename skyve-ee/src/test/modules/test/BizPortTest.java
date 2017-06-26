package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.messages.UploadException;
import org.skyve.util.Util;

import modules.ModulesUtil;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;
import util.AbstractH2Test;

public class BizPortTest extends AbstractH2Test {
	@Test
	public void testStandardExportSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 3);
		test = p.save(test);
		BizPortWorkbook workbook = ModulesUtil.standardBeanBizExport(m.getName(), messd.getName(), test);
		UploadException problems = new UploadException();
		ModulesUtil.standardBeanBizImport(workbook, problems);
		Assert.assertFalse(problems.hasErrors());
	}

	@Test
	public void testStandardExportJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 3);
		test = p.save(test);
		BizPortWorkbook workbook = ModulesUtil.standardBeanBizExport(m.getName(), mejsd.getName(), test);
		UploadException problems = new UploadException();
		ModulesUtil.standardBeanBizImport(workbook, problems);
		Assert.assertFalse(problems.hasErrors());
	}

	@Test
	public void testStandardExportSubclassedSingleStrategy() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 3);
		test = p.save(test);
		BizPortWorkbook workbook = ModulesUtil.standardBeanBizExport(m.getName(), msssd.getName(), test);
		UploadException problems = new UploadException();
		ModulesUtil.standardBeanBizImport(workbook, problems);
		Assert.assertFalse(problems.hasErrors());
	}

	@Test
	public void testStandardExportSubclassedJoinedStrategy() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 3);
		test = p.save(test);
		BizPortWorkbook workbook = ModulesUtil.standardBeanBizExport(m.getName(), msjsd.getName(), test);
		UploadException problems = new UploadException();
		ModulesUtil.standardBeanBizImport(workbook, problems);
		Assert.assertFalse(problems.hasErrors());
	}
}
