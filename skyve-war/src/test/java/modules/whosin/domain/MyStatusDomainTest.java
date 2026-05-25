package modules.whosin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class MyStatusDomainTest {

	private MyStatus bean;

	@BeforeEach
	void setUp() {
		bean = new MyStatus();
	}

	@Test
	void bizModuleIsWhosin() {
		assertEquals("whosin", bean.getBizModule());
	}

	@Test
	void bizDocumentIsMyStatus() {
		assertEquals("MyStatus", bean.getBizDocument());
	}

	@Test
	void moduleNameConstant() {
		assertEquals("whosin", MyStatus.MODULE_NAME);
	}

	@Test
	void documentNameConstant() {
		assertEquals("MyStatus", MyStatus.DOCUMENT_NAME);
	}

	@Test
	void myStaffInitiallyNull() {
		assertNull(bean.getMyStaff());
	}

	@Test
	void isNotExistsWhenMyStaffNull() {
		assertFalse(bean.isExists());
		assertTrue(bean.isNotExists());
	}

	@Test
	void isExistsWhenMyStaffSet() {
		bean.setMyStaff(new Staff());
		assertTrue(bean.isExists());
		assertFalse(bean.isNotExists());
	}

	@Test
	void getBizKeyNotNull() {
		assertNotNull(bean.getBizKey());
	}
}
