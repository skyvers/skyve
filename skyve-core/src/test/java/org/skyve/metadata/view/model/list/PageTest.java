package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;

class PageTest {

	@Test
	@SuppressWarnings("static-method")
	void setTotalRowsRoundtrip() {
		Page page = new Page();
		page.setTotalRows(100L);
		assertEquals(100L, page.getTotalRows());
	}

	@Test
	@SuppressWarnings("static-method")
	void rowsNullByDefault() {
		Page page = new Page();
		assertNull(page.getRows());
	}

	@Test
	@SuppressWarnings("static-method")
	void setRowsRoundtrip() {
		Page page = new Page();
		List<Bean> rows = new ArrayList<>();
		page.setRows(rows);
		assertEquals(rows, page.getRows());
	}

	@Test
	@SuppressWarnings("static-method")
	void summaryNullByDefault() {
		Page page = new Page();
		assertNull(page.getSummary());
	}

	@Test
	@SuppressWarnings("static-method")
	void setSummaryRoundtrip() {
		Page page = new Page();
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.DOCUMENT_ID, "id-1");
		Bean summary = new DynamicBean("admin", "User", props);
		page.setSummary(summary);
		assertSame(summary, page.getSummary());
	}
}
