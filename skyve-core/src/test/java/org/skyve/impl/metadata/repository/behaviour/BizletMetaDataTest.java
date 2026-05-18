package org.skyve.impl.metadata.repository.behaviour;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class BizletMetaDataTest {

	@Test
	@SuppressWarnings("static-method")
	void documentationNullByDefault() {
		BizletMetaData bmd = new BizletMetaData();
		assertNull(bmd.getDocumentation());
	}

	@Test
	@SuppressWarnings("static-method")
	void setDocumentationRoundtrip() {
		BizletMetaData bmd = new BizletMetaData();
		bmd.setDocumentation("Some docs");
		assertThat(bmd.getDocumentation(), is("Some docs"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDocumentationBlankBecomesNull() {
		BizletMetaData bmd = new BizletMetaData();
		bmd.setDocumentation("   ");
		assertNull(bmd.getDocumentation());
	}

	@Test
	@SuppressWarnings("static-method")
	void lastModifiedMillisMaxLongByDefault() {
		BizletMetaData bmd = new BizletMetaData();
		assertEquals(Long.MAX_VALUE, bmd.getLastModifiedMillis());
	}

	@Test
	@SuppressWarnings("static-method")
	void setLastModifiedMillisRoundtrip() {
		BizletMetaData bmd = new BizletMetaData();
		bmd.setLastModifiedMillis(12345L);
		assertEquals(12345L, bmd.getLastModifiedMillis());
	}

	@Test
	@SuppressWarnings("static-method")
	void setLastCheckedMillisRoundtrip() {
		BizletMetaData bmd = new BizletMetaData();
		long now = System.currentTimeMillis();
		bmd.setLastCheckedMillis(now);
		assertEquals(now, bmd.getLastCheckedMillis());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertReturnsSelf() {
		BizletMetaData bmd = new BizletMetaData();
		assertThat(bmd.convert("bizlet"), is(bmd));
	}

	@Test
	@SuppressWarnings("static-method")
	void propertiesNotNull() {
		BizletMetaData bmd = new BizletMetaData();
		assertNotNull(bmd.getProperties());
	}
}
