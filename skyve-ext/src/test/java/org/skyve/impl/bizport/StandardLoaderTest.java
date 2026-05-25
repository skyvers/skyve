package org.skyve.impl.bizport;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.Set;

import org.junit.Test;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.model.document.Document;

public class StandardLoaderTest {

	private static BizPortWorkbook mockWorkbook() {
		BizPortWorkbook wb = mock(BizPortWorkbook.class);
		when(wb.getSheetKeys()).thenReturn(new HashSet<>());
		return wb;
	}

	@Test
	public void constructorSetsWorkbookAndProblems() {
		BizPortWorkbook wb = mockWorkbook();
		UploadException problems = new UploadException();
		StandardLoader loader = new StandardLoader(wb, problems);
		assertNotNull(loader);
	}

	@Test
	public void getBeanKeysEmptyAfterConstruction() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		Set<String> keys = new HashSet<>();
		loader.getBeanKeys().forEach(keys::add);
		assertTrue(keys.isEmpty());
	}

	@Test
	public void getBeanReturnsNullForUnknownKey() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		assertNull(loader.getBean("unknown.key"));
	}

	@Test
	public void putBeanAndGetBeanRoundTrips() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		Bean bean = mock(Bean.class);
		loader.putBean("test.module.doc.1", bean);
		assertEquals(bean, loader.getBean("test.module.doc.1"));
	}

	@Test
	public void putMultipleBeansAndGetEach() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		Bean bean1 = mock(Bean.class);
		Bean bean2 = mock(Bean.class);
		loader.putBean("key1", bean1);
		loader.putBean("key2", bean2);
		assertEquals(bean1, loader.getBean("key1"));
		assertEquals(bean2, loader.getBean("key2"));
	}

	@Test
	public void getBeanKeysReflectsAddedBeans() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		Bean bean = mock(Bean.class);
		loader.putBean("module.doc.id", bean);
		Set<String> keys = new HashSet<>();
		loader.getBeanKeys().forEach(keys::add);
		assertTrue(keys.contains("module.doc.id"));
	}

	@Test
	public void getSheetRowIdFromBizIdReturnsNullForUnknown() {
		StandardLoader loader = new StandardLoader(mockWorkbook(), new UploadException());
		assertNull(loader.getSheetRowIdFromBizId("nonexistent-bizid"));
	}

	@Test
	public void createSheetKeyFormatsAsModuleDocumentSheetId() {
		Document doc = mock(Document.class);
		when(doc.getOwningModuleName()).thenReturn("testModule");
		when(doc.getName()).thenReturn("TestDocument");
		String key = StandardLoader.createSheetKey(doc, "row1");
		assertEquals("testModule.TestDocument.row1", key);
	}

	@Test
	public void createSheetKeyWithIntegerSheetId() {
		Document doc = mock(Document.class);
		when(doc.getOwningModuleName()).thenReturn("admin");
		when(doc.getName()).thenReturn("User");
		String key = StandardLoader.createSheetKey(doc, Integer.valueOf(42));
		assertEquals("admin.User.42", key);
	}

	@Test
	public void createSheetKeyWithEmptyModuleAndDocument() {
		Document doc = mock(Document.class);
		when(doc.getOwningModuleName()).thenReturn("");
		when(doc.getName()).thenReturn("");
		String key = StandardLoader.createSheetKey(doc, "id");
		assertEquals("..id", key);
	}
}
