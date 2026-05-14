package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;

public class PageTest {

	@Test
	@SuppressWarnings("static-method")
	public void setTotalRowsRoundtrip() {
		Page page = new Page();
		page.setTotalRows(100L);
		assertEquals(100L, page.getTotalRows());
	}

	@Test
	@SuppressWarnings("static-method")
	public void rowsNullByDefault() {
		Page page = new Page();
		assertNull(page.getRows());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setRowsRoundtrip() {
		Page page = new Page();
		List<Bean> rows = new ArrayList<>();
		page.setRows(rows);
		assertEquals(rows, page.getRows());
	}

	@Test
	@SuppressWarnings("static-method")
	public void summaryNullByDefault() {
		Page page = new Page();
		assertNull(page.getSummary());
	}
}
