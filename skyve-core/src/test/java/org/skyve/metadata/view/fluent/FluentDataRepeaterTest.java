package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;

public class FluentDataRepeaterTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesDataRepeater() {
		FluentDataRepeater dr = new FluentDataRepeater();
		assertNotNull(dr.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		DataRepeater repeater = new DataRepeater();
		FluentDataRepeater dr = new FluentDataRepeater(repeater);
		assertSame(repeater, dr.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void showColumnHeadersReturnsSelf() {
		FluentDataRepeater dr = new FluentDataRepeater();
		FluentDataRepeater result = dr.showColumnHeaders(Boolean.TRUE);
		assertSame(dr, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void showGridReturnsSelf() {
		FluentDataRepeater dr = new FluentDataRepeater();
		FluentDataRepeater result = dr.showGrid(Boolean.FALSE);
		assertSame(dr, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void addBoundColumnReturnsSelf() {
		FluentDataRepeater dr = new FluentDataRepeater();
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().binding("name");
		FluentDataRepeater result = dr.addBoundColumn(col);
		assertSame(dr, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void addBoundColumnAtIndexReturnsSelf() {
		FluentDataRepeater dr = new FluentDataRepeater();
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn().binding("name");
		FluentDataRepeater result = dr.addBoundColumn(0, col);
		assertSame(dr, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void addContainerColumnReturnsSelf() {
		FluentDataRepeater dr = new FluentDataRepeater();
		FluentDataGridContainerColumn col = new FluentDataGridContainerColumn();
		FluentDataRepeater result = dr.addContainerColumn(col);
		assertSame(dr, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void clearColumnsReturnsSelf() {
		FluentDataRepeater dr = new FluentDataRepeater();
		dr.addBoundColumn(new FluentDataGridBoundColumn().binding("x"));
		FluentDataRepeater result = dr.clearColumns();
		assertSame(dr, result);
	}
}
