package org.skyve.impl.web.faces.views;

import java.io.IOException;
import java.io.InputStream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.primefaces.model.file.UploadedFile;

import jakarta.faces.context.FacesContext;

class AbstractUploadTest {
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

	@BeforeEach
	@SuppressWarnings("deprecation")
	void before() {
		MockitoAnnotations.initMocks(this);
	}
	
	@AfterEach
	void after() throws Exception {
		// no-op
	}
	
	@Test
	@SuppressWarnings("java:S5961")
	void testUploadDefaultFiles() {
		ContentUploadView content = new ContentUploadView();
		TestUploadedFile file = new TestUploadedFile();
		
		Assertions.assertTrue(content.validFile(file.test("test.txt", 10485760), fc)); // largest
		Assertions.assertFalse(content.validFile(file.test("test.txt", 10485761), fc)); // too big
		Assertions.assertFalse(content.validFile(file.test(".htaccess", 10485760), fc)); // starts with .
		Assertions.assertFalse(content.validFile(file.test(".exe", 10485760), fc)); // exe is blocked
		Assertions.assertFalse(content.validFile(file.test("test.bas", 10485760), fc)); // bas is blocked
		Assertions.assertFalse(content.validFile(file.test("test.exe", 10485760), fc)); // exe is blocked
		Assertions.assertFalse(content.validFile(file.test("test.js", 10485760), fc)); // js is blocked
		Assertions.assertTrue(content.validFile(file.test("test.json", 10485760), fc)); // json is not blocked

		Assertions.assertTrue(content.validFile(file.test("test/test.txt", 0), fc));
		Assertions.assertFalse(content.validFile(file.test("test/test.exe", 0), fc)); // exe is blocked
		Assertions.assertFalse(content.validFile(file.test("test/.htaccess", 0), fc)); // starts with .
		Assertions.assertFalse(content.validFile(file.test("test/test.js", 0), fc)); // js is blocked
		Assertions.assertTrue(content.validFile(file.test("test/test.json", 0), fc)); // json is not blocked

		Assertions.assertTrue(content.validFile(file.test("/Users/test/test.txt", 0), fc));
		Assertions.assertFalse(content.validFile(file.test("/Users/test/test.exe", 0), fc)); // exe is blocked
		Assertions.assertFalse(content.validFile(file.test("/Users/test/.htaccess", 0), fc)); // starts with .
		Assertions.assertFalse(content.validFile(file.test("/Users/test/test.js", 0), fc)); // js is blocked
		Assertions.assertTrue(content.validFile(file.test("/Users/test/test.json", 0), fc)); // json is not blocked

		Assertions.assertTrue(content.validFile(file.test("C:/test.txt", 0), fc));
		Assertions.assertFalse(content.validFile(file.test("C:/test.exe", 0), fc)); // exe is blocked
		Assertions.assertFalse(content.validFile(file.test("C:/test/.htaccess", 0), fc)); // starts with .
		Assertions.assertFalse(content.validFile(file.test("C:/test.js", 0), fc)); // js is blocked
		Assertions.assertTrue(content.validFile(file.test("C:/test.json", 0), fc)); // json is not blocked

		Assertions.assertTrue(content.validFile(file.test("/C:/test/test.txt", 0), fc));
		Assertions.assertFalse(content.validFile(file.test("/C:/test/test.exe", 0), fc)); // exe is blocked
		Assertions.assertFalse(content.validFile(file.test("/C:/test/.htaccess", 0), fc)); // starts with .
		Assertions.assertFalse(content.validFile(file.test("C:/test/test.js", 0), fc)); // js is blocked
		Assertions.assertTrue(content.validFile(file.test("C:/test/test.json", 0), fc)); // json is not blocked

		Assertions.assertTrue(content.validFile(file.test("C:\\test.txt", 0), fc));
		Assertions.assertFalse(content.validFile(file.test("C:\\test.exe", 0), fc)); // exe is blocked
		Assertions.assertFalse(content.validFile(file.test("C:\\.htaccess", 0), fc)); // starts with .
		Assertions.assertFalse(content.validFile(file.test("C:\\test.js", 0), fc)); // js is blocked
		Assertions.assertTrue(content.validFile(file.test("C:\\test.json", 0), fc)); // json is not blocked

		Assertions.assertTrue(content.validFile(file.test("C:\\test\\test.txt", 0), fc));
		Assertions.assertFalse(content.validFile(file.test("C:\\test\\test.exe", 0), fc)); // exe is blocked
		Assertions.assertFalse(content.validFile(file.test("C:\\test\\.htaccess", 0), fc)); // starts with .
		Assertions.assertFalse(content.validFile(file.test("C:\\test\\test.js", 0), fc)); // js is blocked
		Assertions.assertTrue(content.validFile(file.test("C:\\test\\test.json", 0), fc)); // json is not blocked
	}
	
	@Test
	@SuppressWarnings("java:S5961")
	void testBizPortDefaultFiles() {
		BizportImportView bizport = new BizportImportView();
		TestUploadedFile file = new TestUploadedFile();
		
		Assertions.assertTrue(bizport.validFile(file.test("test.xls", 10485760), fc)); // largest
		Assertions.assertFalse(bizport.validFile(file.test("test.xls", 10485761), fc)); // too big
		Assertions.assertTrue(bizport.validFile(file.test("test.xlsx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("test.txt", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("test.xlsxx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test(".htaccess", 0), fc)); // starts with .
		Assertions.assertFalse(bizport.validFile(file.test(".exe", 0), fc)); // exe is blocked

		Assertions.assertTrue(bizport.validFile(file.test("test/test.xls", 0), fc));
		Assertions.assertTrue(bizport.validFile(file.test("test/test.xlsx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("test/test.txt", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("test/test.xlsxx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("test/.htaccess", 0), fc)); // starts with .

		Assertions.assertTrue(bizport.validFile(file.test("/Users/test/test.xls", 0), fc));
		Assertions.assertTrue(bizport.validFile(file.test("/Users/test/test.xlsx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("/Users/test/test.txt", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("/Users/test/test.xlsxx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("/Users/test/.htaccess", 0), fc)); // starts with .

		Assertions.assertTrue(bizport.validFile(file.test("C:/test.xls", 0), fc));
		Assertions.assertTrue(bizport.validFile(file.test("C:/test.xlsx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("C:/test.txt", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("C:/test.xlsxx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("C:/test/.htaccess", 0), fc)); // starts with .

		Assertions.assertTrue(bizport.validFile(file.test("/C:/test/test.xls", 0), fc));
		Assertions.assertTrue(bizport.validFile(file.test("/C:/test/test.xlsx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("/C:/test/test.txt", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("/C:/test/test.xlsxx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("/C:/test/.htaccess", 0), fc)); // starts with .

		Assertions.assertTrue(bizport.validFile(file.test("C:\\test.xls", 0), fc));
		Assertions.assertTrue(bizport.validFile(file.test("C:\\test.xlsx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("C:\\test.txt", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("C:\\\\test.xlsxx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("C:\\.htaccess", 0), fc)); // starts with .

		Assertions.assertTrue(bizport.validFile(file.test("C:\\test\\test.xls", 0), fc));
		Assertions.assertTrue(bizport.validFile(file.test("C:\\test\\test.xlsx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("C:\\test\\test.txt", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("C:\\test\\test.xlsxx", 0), fc));
		Assertions.assertFalse(bizport.validFile(file.test("C:\\test\\.htaccess", 0), fc)); // starts with .
	}
}
