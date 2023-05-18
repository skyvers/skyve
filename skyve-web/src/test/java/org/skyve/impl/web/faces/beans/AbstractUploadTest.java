package org.skyve.impl.web.faces.beans;

import java.io.IOException;
import java.io.InputStream;

import javax.faces.context.FacesContext;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.primefaces.model.file.UploadedFile;

public class AbstractUploadTest {
	private static class TestUploadedFile implements UploadedFile {
		private String fileName;
		private long size;
		
		@Override
		public String getFileName() {
			return fileName;
		}

		@Override
		public InputStream getInputStream() throws IOException {
			return null;
		}

		@Override
		public byte[] getContent() {
			return null;
		}

		@Override
		public String getContentType() {
			return null;
		}

		@Override
		public long getSize() {
			return size;
		}

		@Override
		public void write(String filePath) throws Exception {
			// nothing to see here
		}
		
		@Override
		public void delete() throws IOException {
			// nothing to see here
		}

		public TestUploadedFile test(@SuppressWarnings("hiding") String fileName,
										@SuppressWarnings("hiding") long size) {
			this.fileName = fileName;
			this.size = size;
			return this;
		}
	}
	
	@Mock
	private FacesContext fc;

	@Before
	public void before() throws Exception {
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void testUploadDefaultFiles() {
		ContentUpload content = new ContentUpload();
		TestUploadedFile file = new TestUploadedFile();
		
		Assert.assertTrue(content.validFile(file.test("test.txt", 10485760), fc)); // largest
		Assert.assertFalse(content.validFile(file.test("test.txt", 10485761), fc)); // too big
		Assert.assertFalse(content.validFile(file.test(".htaccess", 10485760), fc)); // starts with .
		Assert.assertFalse(content.validFile(file.test(".exe", 10485760), fc)); // exe is blocked
		Assert.assertFalse(content.validFile(file.test("test.bas", 10485760), fc)); // bas is blocked
		Assert.assertFalse(content.validFile(file.test("test.exe", 10485760), fc)); // exe is blocked
		Assert.assertFalse(content.validFile(file.test("test.js", 10485760), fc)); // js is blocked
		Assert.assertTrue(content.validFile(file.test("test.json", 10485760), fc)); // json is not blocked

		Assert.assertTrue(content.validFile(file.test("test/test.txt", 0), fc));
		Assert.assertFalse(content.validFile(file.test("test/test.exe", 0), fc)); // exe is blocked
		Assert.assertFalse(content.validFile(file.test("test/.htaccess", 0), fc)); // starts with .
		Assert.assertFalse(content.validFile(file.test("test/test.js", 0), fc)); // js is blocked
		Assert.assertTrue(content.validFile(file.test("test/test.json", 0), fc)); // json is not blocked

		Assert.assertTrue(content.validFile(file.test("/Users/test/test.txt", 0), fc));
		Assert.assertFalse(content.validFile(file.test("/Users/test/test.exe", 0), fc)); // exe is blocked
		Assert.assertFalse(content.validFile(file.test("/Users/test/.htaccess", 0), fc)); // starts with .
		Assert.assertFalse(content.validFile(file.test("/Users/test/test.js", 0), fc)); // js is blocked
		Assert.assertTrue(content.validFile(file.test("/Users/test/test.json", 0), fc)); // json is not blocked

		Assert.assertTrue(content.validFile(file.test("C:/test.txt", 0), fc));
		Assert.assertFalse(content.validFile(file.test("C:/test.exe", 0), fc)); // exe is blocked
		Assert.assertFalse(content.validFile(file.test("C:/test/.htaccess", 0), fc)); // starts with .
		Assert.assertFalse(content.validFile(file.test("C:/test.js", 0), fc)); // js is blocked
		Assert.assertTrue(content.validFile(file.test("C:/test.json", 0), fc)); // json is not blocked

		Assert.assertTrue(content.validFile(file.test("/C:/test/test.txt", 0), fc));
		Assert.assertFalse(content.validFile(file.test("/C:/test/test.exe", 0), fc)); // exe is blocked
		Assert.assertFalse(content.validFile(file.test("/C:/test/.htaccess", 0), fc)); // starts with .
		Assert.assertFalse(content.validFile(file.test("C:/test/test.js", 0), fc)); // js is blocked
		Assert.assertTrue(content.validFile(file.test("C:/test/test.json", 0), fc)); // json is not blocked

		Assert.assertTrue(content.validFile(file.test("C:\\test.txt", 0), fc));
		Assert.assertFalse(content.validFile(file.test("C:\\test.exe", 0), fc)); // exe is blocked
		Assert.assertFalse(content.validFile(file.test("C:\\.htaccess", 0), fc)); // starts with .
		Assert.assertFalse(content.validFile(file.test("C:\\test.js", 0), fc)); // js is blocked
		Assert.assertTrue(content.validFile(file.test("C:\\test.json", 0), fc)); // json is not blocked

		Assert.assertTrue(content.validFile(file.test("C:\\test\\test.txt", 0), fc));
		Assert.assertFalse(content.validFile(file.test("C:\\test\\test.exe", 0), fc)); // exe is blocked
		Assert.assertFalse(content.validFile(file.test("C:\\test\\.htaccess", 0), fc)); // starts with .
		Assert.assertFalse(content.validFile(file.test("C:\\test\\test.js", 0), fc)); // js is blocked
		Assert.assertTrue(content.validFile(file.test("C:\\test\\test.json", 0), fc)); // json is not blocked
	}
	
	@Test
	public void testBizPortDefaultFiles() {
		BizportImport bizport = new BizportImport();
		TestUploadedFile file = new TestUploadedFile();
		
		Assert.assertTrue(bizport.validFile(file.test("test.xls", 10485760), fc)); // largest
		Assert.assertFalse(bizport.validFile(file.test("test.xls", 10485761), fc)); // too big
		Assert.assertTrue(bizport.validFile(file.test("test.xlsx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("test.txt", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("test.xlsxx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test(".htaccess", 0), fc)); // starts with .
		Assert.assertFalse(bizport.validFile(file.test(".exe", 0), fc)); // exe is blocked

		Assert.assertTrue(bizport.validFile(file.test("test/test.xls", 0), fc));
		Assert.assertTrue(bizport.validFile(file.test("test/test.xlsx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("test/test.txt", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("test/test.xlsxx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("test/.htaccess", 0), fc)); // starts with .

		Assert.assertTrue(bizport.validFile(file.test("/Users/test/test.xls", 0), fc));
		Assert.assertTrue(bizport.validFile(file.test("/Users/test/test.xlsx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("/Users/test/test.txt", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("/Users/test/test.xlsxx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("/Users/test/.htaccess", 0), fc)); // starts with .

		Assert.assertTrue(bizport.validFile(file.test("C:/test.xls", 0), fc));
		Assert.assertTrue(bizport.validFile(file.test("C:/test.xlsx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("C:/test.txt", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("C:/test.xlsxx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("C:/test/.htaccess", 0), fc)); // starts with .

		Assert.assertTrue(bizport.validFile(file.test("/C:/test/test.xls", 0), fc));
		Assert.assertTrue(bizport.validFile(file.test("/C:/test/test.xlsx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("/C:/test/test.txt", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("/C:/test/test.xlsxx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("/C:/test/.htaccess", 0), fc)); // starts with .

		Assert.assertTrue(bizport.validFile(file.test("C:\\test.xls", 0), fc));
		Assert.assertTrue(bizport.validFile(file.test("C:\\test.xlsx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("C:\\test.txt", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("C:\\\\test.xlsxx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("C:\\.htaccess", 0), fc)); // starts with .

		Assert.assertTrue(bizport.validFile(file.test("C:\\test\\test.xls", 0), fc));
		Assert.assertTrue(bizport.validFile(file.test("C:\\test\\test.xlsx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("C:\\test\\test.txt", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("C:\\test\\test.xlsxx", 0), fc));
		Assert.assertFalse(bizport.validFile(file.test("C:\\test\\.htaccess", 0), fc)); // starts with .
	}
}
