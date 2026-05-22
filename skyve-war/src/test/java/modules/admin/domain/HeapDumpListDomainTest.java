package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class HeapDumpListDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		HeapDumpList bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(HeapDumpList.MODULE_NAME, HeapDumpList.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		HeapDumpList bean = HeapDumpList.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("HeapDumpList", bean.getBizDocument());
	}

	@Test
	void refreshSetAndGet() throws Exception {
		HeapDumpList bean = HeapDumpList.newInstance();
		bean.setRefresh(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getRefresh());
	}

	@Test
	void selectedNameSetAndGet() throws Exception {
		HeapDumpList bean = HeapDumpList.newInstance();
		bean.setSelectedName("heap_20240101.hprof");
		assertEquals("heap_20240101.hprof", bean.getSelectedName());
	}
}
